package org.fusionatlas.enumerators.queues
import net.tqft.toolkit.Extractors.Double
import net.tqft.toolkit.collections.Queues
import net.tqft.toolkit.amazon.SQSQueues
import net.tqft.toolkit.amazon.StackOfQueues
import net.tqft.toolkit.amazon.S3Stack
import net.tqft.toolkit.amazon.QueueServiceDefaultAccount
import net.tqft.toolkit.collections.Stack
import org.fusionatlas.graphs.FusionGraph
import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import org.fusionatlas.graphs.PersistentGraph
import net.tqft.toolkit.Throttle
import net.tqft.toolkit.collections.Queue

object JobQueue {
  def jobToString[_](p: (Double, PersistentGraph[_])) = p._1 + "@" + p._2
  def stringToJob(s: String): (Double, PersistentGraph[_]) = {
    s.split('@').toList match {
      case Double(d) :: PairOfBigraphsWithDuals(h) :: Nil => (d, h)
      case Double(d) :: FusionGraph(h) :: Nil => (d, h)
      case _ => throw new IllegalArgumentException("Received a malformed message from the queue: " + s)
    }
  }

  def sqs(priorities: Traversable[Int] = 0 until 10) = {
    import Queues._
    new SQSQueues(QueueServiceDefaultAccount()).getPriorityQueue("fusionatlas-enumeration-requests", 3600, priorities).transform(stringToJob _, jobToString _).orderBy(_._2.priority)
  }

  def s3stack() = {
    import Stack._
    import Queues._
    S3Stack("fusionatlas-enumeration-requests").transform(stringToJob _, jobToString _).asQueue
  }

  def sqsStack() = {
    import Queues._
    StackOfQueues("fusionatlas-enumeration-requests-", updatePeriod = Some(30000)).transform(stringToJob _, jobToString _)
  }

  def apply(): Queue[(Double, PersistentGraph[_])] = {
    import Queues._
    s3stack().retrying(Throttle.linearBackoff(5000))
  }
}

