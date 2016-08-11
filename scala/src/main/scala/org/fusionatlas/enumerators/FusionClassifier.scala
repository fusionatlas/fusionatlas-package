package org.fusionatlas.enumerators

import org.fusionatlas.graphs.obstructions.FusionObstructions
import org.fusionatlas.graphs._
import org.fusionatlas.enumerators.scheduling._
import net.tqft.toolkit.Logging

abstract class FusionClassifier(d: Double, roots: List[FusionGraph], ignoredWeeds: List[FusionGraph]) extends Classifier[FusionGraph](d, roots, ignoredWeeds) with FusionObstructions
abstract class DatabaseFusionClassifier(d: Double, roots: List[FusionGraph], ignoredWeeds: List[FusionGraph]) extends FusionClassifier(d, roots, ignoredWeeds) {
  val enumerator = DatabaseFusionEnumerator
}

class FutureFusionClassifier(d: Double, roots: List[FusionGraph], ignoredWeeds: List[FusionGraph] = Nil) extends DatabaseFusionClassifier(d, roots, ignoredWeeds) with FutureScheduler[FusionGraph]
class QueueFusionClassifier(d: Double, roots: List[FusionGraph], ignoredWeeds: List[FusionGraph] = Nil) extends DatabaseFusionClassifier(d, roots, ignoredWeeds) with SQSScheduler[FusionGraph]
class LocalFusionClassifer(d: Double, roots: List[FusionGraph], ignoredWeeds: List[FusionGraph] = Nil) extends FusionClassifier(d, roots, ignoredWeeds) with ImmediateScheduler[FusionGraph] {
  val enumerator = InMemoryFusionEnumerator
}

object Norms {
  val epsilon = 0.00001
  val IX = 2.18890
  val sqrt5 = 2.23607
  val sqrt3plusSqrt5 = 2.28826
  val CMS = 2.30303
  val hat = 2.24698
  val sqrt6 = 2.44949
}

object FusionClassifierApplication {
  import Norms._

  val A2 = FusionGraph("fg0v0v1v0v0")
  val A3 = FusionGraph("fg0v0v1v0v0v1v0v0")
  val T2 = FusionGraph("fg0v0v1v0v1")

  val ignoredWeeds: List[FusionGraph] = List(A3)

  def main(args: Array[String]): Unit = {
    import net.tqft.toolkit.mathematica.MathematicaExpression._

    val classifier = new FutureFusionClassifier(IX, List(A2), List(A3))

    var pause = 0
    while (!classifier.completed) {
      println("Completed: " + classifier.completed)
      val weeds = classifier.weeds()
      val hay = classifier.hay
      val vines = classifier.vines().toList
      println("Weeds (" + weeds.size + "): " + weeds)
      println("Hay (" + hay.size + "): " + hay)
      println("Vines (" + vines.size + "): " + vines)

      classifier.schedulePending()

      Thread.sleep(pause)
      pause = pause + 100
      classifier.update()

    }
  }

}