package org.fusionatlas.permutations

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PermutationsTest extends FlatSpec with ShouldMatchers {

  import net.tqft.toolkit.permutations.Permutations._

  "Involutions" should "choose representatives of the set of involutions mod product permutations groups" in {
    Involutions.ofChunks(List(2, 2)).size should equal(6)
    for (inv <- Involutions.ofChunks(List(4, 3, 2)); p = inv map (_ - 1)) {
      p.sortWith(_ < _) should equal((0 to 8).toList)
      p permute p should equal((0 to 8).toList)
    }
    Involutions.ofChunks(List(3, 2)).size should equal(7)
    Involutions.ofChunks(List(2, 2, 2)).size should equal(24)
    Involutions.ofChunks(List(5, 4, 3, 2, 1)).size should equal(4431)
  }
}