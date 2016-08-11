package org.fusionatlas.enumerators.scheduling

import net.tqft.toolkit.PrintLogging
import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import org.fusionatlas.enumerators.Classifier
import org.fusionatlas.graphs.PersistentGraph
import org.fusionatlas.enumerators.queues.JobQueue
//import net.tqft.toolkit.Scheduler
import net.tqft.toolkit.amazon.S3

trait SQSScheduler[G <: PersistentGraph[G]] extends PrintLogging with NonRepeatingScheduler[G] { this: Classifier[G] =>
  private lazy val queue = {
    // FIXME Scheduler is gone ...
    throw new UnsupportedOperationException
//    Scheduler(10000)(checkQueueNonEmpty)
    JobQueue()
  }

  var lastCheck = System.currentTimeMillis
  def checkQueueNonEmpty {
    if (System.currentTimeMillis - lastCheck > 5000) {
      lastCheck = System.currentTimeMillis
      //    queue.dequeue match {
      S3("fusionatlas-enumeration-requests").keysWithPrefix("", 1).headOption match {
        case None => {
          println("JobQueue appears to be empty, allowing rescheduling!")
          scheduled.clear
        }
        case Some(_) => {
          println("JobQueue non-empty.")
          //        queue.enqueue(x)
        }
      }
    }
  }

  override def schedule(h: G) {
    import scala.actors.Futures.future
    if (propose(h)) {
      info("In the future, I'll send this message to the queue: " + d + "@" + h)
      future {
        info("Sending message to queue: " + d + "@" + h)
        queue.enqueue((d, h))
      }
    }
  }
}
