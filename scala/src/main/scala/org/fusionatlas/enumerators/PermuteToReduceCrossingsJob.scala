package org.fusionatlas.enumerators
import org.fusionatlas.graphs.PairOfBigraphsWithDuals

object PermuteToReduceCrossingsJob {
  def main(args: Array[String]): Unit = {
    val vinesAndWeeds = S3PairMaps.vinesAndWeeds

    def process(p: (Double, PairOfBigraphsWithDuals)) {
      val (d, g) = p
      println("looking up: " + (d, g))
      val newg = g.permuteToReduceCrossings(true)
      val (gVines, gWeeds) = vinesAndWeeds((d, g))
      val newVines = gVines.map(_.permuteToReduceCrossings(true))
      val newWeeds = gWeeds.map(_.permuteToReduceCrossings(true))
      if (g != newg || newVines != gVines || newWeeds != gWeeds) {
        println("updating the store")
        vinesAndWeeds -= ((d, g))
        vinesAndWeeds += (((d, newg), (newVines, newWeeds)))
      }
    }

    //    java.lang.System.setProperty("actors.enableForkJoin", "true")
    //    
    import net.tqft.toolkit.collections.Iterables._
    //    vinesAndWeeds.keys.consume(process _, 16)
    //
    for (k <- vinesAndWeeds.keys) process(k)
  }
}
