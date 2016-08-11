package org.fusionatlas.enumerators.scheduling

import org.fusionatlas.enumerators.Classifier
import org.fusionatlas.graphs.PersistentGraph
import scala.actors.Actor
import scala.actors.scheduler.ForkJoinScheduler
import net.tqft.toolkit.PrintLogging

//object FutureScheduler extends PrintLogging { obj =>
//  def run(f: => Unit) {
//    try {
//      new Actor() {
//        def act() {
//          try {
//            f
//          } catch {
//            case e => error("Caught an exception while running a task: ", e)
//          }
//        }
//      }.start
//    } catch {
//      case e: Exception => error("Caught an exception while scheduling a task: ", e)
//    }
//  }
//}
//
trait FutureScheduler[G <: PersistentGraph[G]] extends PrintLogging with NonRepeatingScheduler[G] { this: Classifier[G] =>
  import scala.actors.Futures.future

  override def schedule(h: G) {
    if (propose(h)) {
      info("In the future, I'll enumerate this: " + d + "@" + h)
      future {
        info("Enumerating: " + d + "@" + h)
        enumerator.extend(d, h)
        complete(h)
      }
    }
  }
}