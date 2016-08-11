package net.tqft.toolkit.amazon
import net.tqft.toolkit.collections.Queue
import com.xerox.amazonws.sqs2.MessageQueue
import scala.collection.JavaConversions
import net.tqft.toolkit.collections.Queues
import net.tqft.toolkit.Logging
import com.xerox.amazonws.sqs2.SQSException
//import net.tqft.toolkit.Scheduler

object StackOfQueues extends Logging { obj =>
  import JavaConversions._
  val queueMaps: scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, SQSQueue]] = new com.google.common.collect.MapMaker().makeMap[String, scala.collection.mutable.Map[String, SQSQueue]]()
  private val queueService = QueueServiceDefaultAccount()
  private val queues = new SQSQueues(queueService)

  def updateQueuePool(prefix: String) {
    info("updating the queue pool")
    val newValues = JavaConversions.asScalaBuffer(queueService.listMessageQueues(prefix)).map(q => q.getUrl.toString -> queues.getQueue(q)).toList.sortBy(_._1).take(10)
    queueMaps(prefix).clear
    queueMaps(prefix) ++= newValues
    for (q <- queueMaps(prefix).values) {
      try {
        info(q.underlyingSQSQueue.getUrl.toString + " has " + q.underlyingSQSQueue.getApproximateNumberOfMessages() + " messages")
      } catch {
        case e: SQSException => info("checking number of messages failed: " + q.underlyingSQSQueue.getUrl)
      }
    }
  }
  def updateAllQueuePools {
    for (prefix <- queueMaps.keys) updateQueuePool(prefix)
  }

  def apply(prefix: String = "", updatePeriod: Option[Int] = None): Queue[String] = {

    if (queueMaps.get(prefix).isEmpty) {
      queueMaps += prefix -> new com.google.common.collect.MapMaker().makeMap[String, SQSQueue]()
    }

    val queuePool = scala.collection.mutable.Map[Int, Queue[String]]()

    def updateQueuePool {
      obj.updateQueuePool(prefix)
      regenerateQueuePool
    }
    def offerQueues(newQueues: Traversable[MessageQueue]) {
      queueMaps(prefix) ++= newQueues.map(q => q.getUrl.toString -> queues.getQueue(q))
      regenerateQueuePool
    }
    def regenerateQueuePool {
      def wrap(p: (Int, SQSQueue)) = {
        val (k, q) = p
        def deleteQueue(dq: MessageQueue)() = {
          try {
            if (dq.getApproximateNumberOfMessages() == 0) {
              info("Attempting to delete queue " + dq.getUrl)
              dq.deleteQueue()
            }
          } catch {
            case e: SQSException => info("Caught an exception while trying to delete a queue: " + e.getMessage())
          }
          queueMaps(prefix) -= dq.getUrl.toString
          regenerateQueuePool
        }
        import Queues._
        val nq = if (k == 0) q else q.notifyAfterFailures(5, deleteQueue(q.underlyingSQSQueue) _)
        k -> nq
      }

      if (queueMaps(prefix).isEmpty) {
        createNewPool
        Thread.sleep(1000)
        updateQueuePool
      }

      queuePool.clear
      queuePool ++= queueMaps(prefix).toList.sortBy(_._1).map(_._2).zipWithIndex.map(_.swap).map(wrap)
    }

    def createNewPool = {
      val name = prefix + (Long.MaxValue - System.currentTimeMillis).toString
      info("Creating new queue " + name)
      val newQueue = queueService.getOrCreateMessageQueue(name)
      offerQueues(Some(newQueue))
      regenerateQueuePool
    }

    val updateActor = updatePeriod.map(p => {
      ???
      // Scheduler was relying on actors, so it's been removed...
//      Scheduler(p)(updateQueuePool)
    })

    import Queues._
    val queue = Queues.priorityQueue(queuePool).orderBy({ _ => 0 })
    queue.triggerOnFirstEnqueue(createNewPool _).triggerOnFirstDequeue(updateQueuePool _).notifyAfterFailures(5, updateQueuePool _)
  }
}