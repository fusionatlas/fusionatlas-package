//package org.fusionatlas.permutations
//
//import net.tqft.toolkit.collections.NonStrictIterable
//import net.tqft.toolkit.arithmetic.BinomialCoefficient
//import net.tqft.toolkit.functions.Memo
//
//object Permutations {
//  type Permutation = List[Int]
//
//  class RichPermutation(p: Permutation) {
//    def permute[A](s: List[A]): List[A] = {
//      for (n <- p) yield { s(n) }
//    }
//    def inverse(): Permutation = {
//      ((0 to p.size - 1) map { k => p.indexWhere(_ == k) }).toList
//    }
//  }
//
//  implicit def Permutation2RichPermutation(p: Permutation) = new RichPermutation(p)
//
//  def identity(n: Int): Permutation = (0 to n - 1).toList
//
//  // return all permutations which take list1 to list2
//  def mapping[A](list1: List[A], list2: List[A]): Iterable[Permutation] = {
//    findOneMapping(list1, list2) match {
//      case Some(q) => preserving(list2) map { (p: Permutation) => p permute q }
//      case None => NonStrictIterable()
//    }
//  }
//  def mappingCached[A](list1: List[A], list2: List[A]): Iterable[Permutation] = {
//    findOneMapping(list1, list2) match {
//      case Some(q) => preservingCached(list2) map { (p: Permutation) => p permute q }
//      case None => NonStrictIterable()
//    }
//  }
//  
//  def findOneMapping[A](list1: List[A], list2: List[A]): Option[Permutation] = findOneMappingWithSameTest(list1, list2)({ case (a, b) => a == b })
//
//  // TODO we only use this for relatively short lists; otherwise it should be tail-recursive
//  def findOneMappingWithSameTest[A](list1: List[A], list2: List[A])(implicit sameTest: (A, A) => Boolean): Option[Permutation] = {
//    list2 match {
//      case list2Head :: list2Tail =>
//        list1.indexWhere(sameTest(_, list2Head)) match {
//          case -1 => None
//          case k => findOneMappingWithSameTest(list1.take(k) ::: list1.drop(k + 1), list2Tail) map { p => k :: (p map { x => if (x >= k) x + 1 else x }) }
//        }
//      case Nil =>
//        list1 match {
//          case Nil => Some(List())
//          case _ => None
//        }
//    }
//  }
//
//  def of(n: Int) = NonStrictIterable.from(if (n <= 6) cachedOf(n) else ofImpl(n))
//
//  def ofImpl(n: Int): Iterable[Permutation] = {
//    if (n == 0) {
//      NonStrictIterable(Nil)
//    } else {
//      NonStrictIterable.iterateUntilNone(identity(n))(
//        p => {
//          val k = ((p zip p.tail) map { case (x, y) => x - y }) lastIndexWhere { _ < 0 }
//          if (k == -1) {
//            None
//          } else {
//            val l = p lastIndexWhere { _ > p(k) }
//            Some(p.take(k) ::: List(p(l)) ::: (p.slice(k + 1, l) ::: List(p(k)) ::: p.drop(l + 1)).reverse)
//          }
//        })
//    }
//  }
//
//  val cachedOf = Memo({ n: Int => ofImpl(n).toList })
//
//  def randomPermutationOf(n: Int) = {
//    scala.util.Random.shuffle((0 until n).toList)
//  }
//  def randomPermutationsOf(n: Int) = NonStrictIterable.continually(randomPermutationOf(n))
//
//  def of[A](l: List[A]): Iterable[List[A]] = {
//    of(l.size) map (_ permute l)
//  }
//
//  def preservingCached[A](list: List[A]) = {
//    val map = scala.collection.mutable.Map[A, Int]()
//    val bf = new scala.collection.mutable.ListBuffer[Int]
//    var counter = -1
//    for(a <- list) {
//      bf += (map.get(a) match {
//        case Some(k) => k
//        case None => {
//          counter = counter + 1
//          map += ((a, counter))
//          counter
//        }
//      })
//    }
//    preservingIntCached(bf.toList)
//  }
//  private val preservingIntCached = Memo({ l: List[Int] => NonStrictIterable.from(preserving(l).toList) })
//  
//  def preserving[A](list: List[A]): Iterable[Permutation] = {
//    val listWithIndices = list zipWithIndex
//    val positions = list.distinct map { x: A => listWithIndices collect { case (`x`, n) => n } }
//    val factors = positions map { l => of(l.size) }
//    val product = factors.reverse.foldLeft(NonStrictIterable(List[Permutation]()))((s0, s1) => s0.flatMap { a => s1.map(_ :: a) })
//
//    def assemblePermutations(positions: List[List[Int]], permutations: List[Permutation]): Permutation = {
//      (positions.flatten.inverse) permute ((positions zip permutations) flatMap { case (p, q) => q permute p })
//    }
//    product map { assemblePermutations(positions, _) }
//  }
//}