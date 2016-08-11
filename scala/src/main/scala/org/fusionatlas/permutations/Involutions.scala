package org.fusionatlas.permutations

object Involutions extends net.tqft.toolkit.permutations.Involutions {

  val ofChunks = net.tqft.toolkit.functions.Memo(ofChunksImplementation _)

  private def ofChunksImplementation(sizes: List[Int]) = {
    import org.fusionatlas.matrices._
    import net.tqft.toolkit.algebra.enumeration.Odometer

    val partialSums = sizes.scanLeft(0)(_ + _)
    val totalSize = partialSums.last

    def buildInvolution(s: SymmetricMatrix): Involution = {
      val m = s.asArray

      val initialVerticesUsed = new Array[Int](sizes.size)
      val finalVerticesUsed = new Array[Int](sizes.size)
      val involution = scala.collection.mutable.Map((for (i <- 1 to totalSize) yield (i, i)): _*)

      for (j <- 0 to sizes.size - 1) {
        // build involutions inside the clump
        for (k <- 1 to m(j)(j)) {
          val offset = partialSums(j) + initialVerticesUsed(j)
          involution put (offset + 1, offset + 2)
          involution put (offset + 2, offset + 1)
          initialVerticesUsed(j) = initialVerticesUsed(j) + 2
        }

        // now build involutions to later clumps
        for (l <- j + 1 to sizes.size - 1) {
          for (k <- 1 to m(j)(l)) {
            val start = partialSums(j + 1) - finalVerticesUsed(j)
            val finish = partialSums(l) + initialVerticesUsed(l) + 1
            involution put (start, finish)
            involution put (finish, start)
            finalVerticesUsed(j) = finalVerticesUsed(j) + 1
            initialVerticesUsed(l) = initialVerticesUsed(l) + 1
          }
        }
      }

      (1 to totalSize).toIndexedSeq map { involution(_) }
    }

    val initial = new SymmetricMatrix(sizes.size)
    val limit = { m: SymmetricMatrix =>
      ((m.asArray zip sizes).zipWithIndex map {
        case ((row, max), index) => row.foldLeft(0)(_ + _) + row(index) <= max
      }).foldLeft(true)(_ && _)
    }
    Odometer(limit)(initial).toList map buildInvolution
  }

}