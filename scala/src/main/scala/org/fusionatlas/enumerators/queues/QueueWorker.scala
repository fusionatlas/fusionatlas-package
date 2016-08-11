package org.fusionatlas.enumerators.queues
import net.tqft.toolkit.collections.Queues
import net.tqft.toolkit.Extractors.Int
import org.fusionatlas.graphs.FusionGraph
import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import org.fusionatlas.graphs.PersistentGraph
import org.fusionatlas.enumerators.DatabasePairEnumerator
import org.fusionatlas.enumerators.DatabaseFusionEnumerator
import net.tqft.toolkit.amazon.StackOfQueues

object QueueWorker {
  def main(args: Array[String]): Unit = {
    val workers = args.toList.headOption.collect { case Int(i) => i } getOrElse (2 * Runtime.getRuntime.availableProcessors)
    val queueWorker = new QueueWorker(workers)
    println("started QueueWorker")
    while({ readLine; true }) {
      StackOfQueues.updateAllQueuePools
    }
  }
}

class QueueWorker(numberOfConsumers: Int) {

  val pairEnumerator = DatabasePairEnumerator
  val fusionEnumerator = DatabaseFusionEnumerator

  def process(job: (Double, PersistentGraph[_])) = job match {
    case (d, fg: FusionGraph) => fusionEnumerator.extend(d, fg)
    case (d, p: PairOfBigraphsWithDuals) => pairEnumerator.extend(d, p)
  }

  // FIXME consume is gone!
//  import Queues._
//  val actors = JobQueue().consume(process _, numberOfConsumers)
//  def stop = for(a <- actors) a ! 'stop
}