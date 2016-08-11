package org.fusionatlas.enumerators

import net.tqft.toolkit.Profiler
import org.fusionatlas.graphs.PairOfBigraphsWithDuals

object EnumeratorProfiler extends Profiler {
  def main(args: Array[String]): Unit = {
    val HaagerupTruncatedPair = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")
    val slowPair = PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p1x0p0x1v1x0x0p0x1x0p0x0x1p0x0x1duals1v1v1x3x2", "bwd1v1v1p1v1x0p1x0p0x1v0x0x1p0x1x0p1x0x0p0x0x1duals1v1v1x3x2")
    val slowPair2 = PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v2x1v2x1", "bwd1v1v1p1p1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v2x1v2x1")
    val memoryPair = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0v1x0p0x1v0x1p1x0v1x0p0x1v0x1p1x0v1x1duals1v1v1x2v1x3x2x4v1x2v2x1v1x2v1", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v1x2v1x2v2x1v1x2v2x1v1x2x3x4")
    val memoryPair2 = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p0x1p0x1duals1v1v1x2")
    val slowPair3 = PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0p0x0x1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p0x1p0x1v1x0x0p1x0x0p1x0x0p0x1x0p0x0x1v1x0x0x0x0p1x0x0x0x0p0x0x0x1x0p0x0x0x1x0p0x0x0x0x1p0x0x0x0x1duals1v1v1x2x3v1x2v1x3x2v1x2x5x6x3x4", "bwd1v1v1p1p1v1x0x0p0x1x0p0x0x1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p0x1p0x1v1x0x0p1x0x0p1x0x0p0x1x0p0x0x1v1x0x0x0x0p1x0x0x0x0p0x0x0x1x0p0x0x0x1x0p0x0x0x0x1p0x0x0x0x1duals1v1v1x2x3v1x2v1x3x2v1x2x5x6x3x4")
    val tooManyWeeds = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1duals1v1v1x2x3", "bwd1v1v1v1p1p1duals1v1v1x2x3")

    val example1 = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v0x1p1x0v0x1p1x0p1x0v0x0x1p0x1x0p1x0x0duals1v1v1x2v1x2v3x2x1", "bwd1v1v1v1p1v1x0p0x1v0x1p0x1p1x0p1x0v0x1x0x0p1x0x0x0p0x0x1x0v0x0x1p0x0x1p0x1x0p0x1x0p1x0x0duals1v1v1x2v1x3x2x4v1x3x2x4x5")

    val slowPair4 = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v0x1p1x0v0x1p0x1p1x0p1x0v0x0x0x1p1x0x0x0duals1v1v1x2v1x2v2x1", "bwd1v1v1v1p1v1x0p0x1v0x1p0x1p1x0p1x0v0x1x0x0p0x0x0x1p1x0x0x0p0x0x1x0v0x0x0x1p0x0x0x1p0x0x1x0p0x1x0x0p1x0x0x0p1x0x0x0duals1v1v1x2v1x3x2x4v1x3x2x5x4x6")

    val sqrt6 = 2.44949
    val sqrt3plusSqrt5 = 2.28826
    val badSeedD = 2.19737

    val enumerator = new Object with PairEnumerator

    //	  def f0 = enumerator.extendForever(/* \sqrt{3+\sqrt{3}} = 2.17534 */  2.19  /* \sqrt{5} = 2.23607 */, HaagerupTruncatedPair).toList
    //
    //	  val period0 = 10
    //	  for(t <- movingTimingAverages(period0)(f0)) println(t)
    //	  // initial average: 3100ms
    //	  // after memoizing neighbours: 1750ms
    //	  // after making the FP eigenvalue estimate smarter: 1000ms
    //	  // after switching to List-based GraphVectors: 520ms        (curiously, Array-based is slower, at 650ms)
    //	  // after using neighbours2 to improve eigenvectors: 480ms
    //	  // after memoizing neighbours2: 460ms
    //	  // after only doing the computation on even vertices: 395ms
    //	  // after moving logger instantiations to companion objects: 375ms
    //	  // changing how neighbours is memo'd: 365ms

    //	  def f1 = enumerator.extend(sqrt3plusSqrt5, slowPair)
    //
    //	  val period1 = 1
    //	  for(t <- movingTimingAverages(period1)(f1)) println(t)
    //	  // slowPair is taking 38 000ms
    //	  // and only 11 000ms after comparing annular multiplicities
    //	  // and about 2 800ms sometime later... (being more careful about involutions)

    //	  def f1 = enumerator.extend(sqrt3plusSqrt5, slowPair2)
    //
    //	  val period1 = 1
    //	  for(t <- movingTimingAverages(period1)(f1)) println(t)
    //	  // slowPair2 is taking 2 800 000ms
    //	  // slowPair2 is taking < 1 200 000ms after switching from Streams to (non-strict!) Iterables
    //	  // and about 16 000ms after dealing with involutions properly!
    //	  // back up to 20 000ms, since we have to do less caching: memory problems...
    //	  // down to 15 500ms, making the old associativity test a bit smarter.

    //	  def f1 = enumerator.extend(badSeedD, memoryPair)
    //
    //	  val period1 = 1
    //	  for(t <- movingTimingAverages(period1)(f1)) println(t)
    //	  // memoryPair is taking 21000

//        	  def f1 = enumerator.extend(sqrt6, memoryPair2)
//        
//        	  val period1 = 1
//        	  for(t <- movingTimingAverages(period1)(f1)) println(t)
    // memoryPair2 is taking 24 000ms, even constrained to 16m of heap!
    // on punica, taking 105 000ms (Sept 5 2011, punica)
    //                    30 000ms, after making annularMultiplicities a lazy val, and memoizing Involutions.ofChunks
    //					27 000ms, making loops a lazy val too, and caching BinomialCoefficients
    //					22 000ms, after storing and uniqueing 'truncateByOne', and have findIsomorphisms used a cached version when recursing
    // back up to 25 500ms, caching was a bad idea (Sept 19, punica)
    //            19 500 ms, with some extra filtering based on associativity
    //            14 500 ms, checking fewer isomorphisms
    //			12 300 ms, after doing some more caching in Permutations
    //			 8 000 ms, less vigorously looking to reduce crossings
    

    //    def f1 = enumerator.extend(sqrt3plusSqrt5, slowPair4)
    //    val period1 = 1
    //    for (t <- movingTimingAverages(period1)(f1)) println(t)
    //
//        	  def f1 = enumerator.extend(sqrt6, tooManyWeeds)
//        	  val period1 = 1
//        	  for(t <- movingTimingAverages(period1)(f1)) println(t)
    // 14 500 ms, (Sept 19, punica)
    // 13 000 ms, removing some ancient debugging statements from findIsomorphisms
    // 10 500 ms, had to use odd-odd-multiplicities a bit more weakly, so the chunking scheme still works
    //  7 000 ms, (2012-03-15, trilobata), removeDuplicates uses a finer invariant

//    def f1 = PairEnumerator.extend(3.60555, PairOfBigraphsWithDuals("bwd1v1p1p1duals1v1x2x3", "bwd1v1p1p1duals1v1x2x3"), Some(35.))
//    val period1 = 1
//    for (t <- movingTimingAverages(period1)(f1)) println(t)
    // 118 000 ms, (20120316, trilobata)
    //  88 000 ms, using parallelChooseRepresentatives
    //  52 000 ms, parallelizing extendBackwards
    
//    val hw = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v0x1p1x0v1x0p0x1p0x1v1x0x0p0x1x0p0x0x1v1x0x0p1x0x0p0x1x0p0x0x1v1x0x0x0p0x1x0x0p0x0x1x0p0x0x0x1duals1v1v1x2v1x2v3x2x1v3x2x1x4", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v1x0x0x0p0x1x0x0p0x0x1x0v1x0x0p0x1x0p0x1x0p0x0x1p0x0x1v0x0x1x0x0p0x0x0x0x1p0x0x0x1x0p1x0x0x0x0v1x0x0x0p0x1x0x0p0x0x1x0p0x0x0x1p0x0x0x1duals1v1v1x2v3x2x1x4v1x2x4x3x5v4x5x3x1x2")
//    def f1 = PairEnumerator.extend(2.24826, hw)
//    val period1 = 1
//    for (t <- movingTimingAverages(period1)(f1)) println(t)
    // 6000 ms (20120502, punica)
    // 5700 ms after sharing eigenvector estimates
    // 5300 ms after simplifying isFPEigenvaluePossiblyBelow
    val hw = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v1x0x0x0p0x1x0x0p0x0x1x0v1x0x0p0x1x0p0x1x0p0x0x1p0x0x1v0x0x1x0x0p0x0x0x0x1p0x0x0x1x0p1x0x0x0x0v1x0x0x0p0x1x0x0p0x0x1x0p0x0x0x1p0x0x0x1duals1v1v1x2v3x2x1x4v1x2x4x3x5v4x5x3x1x2", "bwd1v1v1v1p1v1x0p1x0v0x1p1x0v1x0p0x1p0x1v1x0x0p0x1x0p0x0x1v1x0x0p1x0x0p0x1x0p0x0x1v1x0x0x0p0x1x0x0p0x0x1x0p0x0x0x1duals1v1v1x2v1x2v3x2x1v3x2x1x4")
    def f1 = PairEnumerator.extend(2.25826, hw)
    val period1 = 1
    for (t <- movingTimingAverages(period1)(f1)) println(t)
  }
}

