package net.tqft.toolkit.collections

object SynchronizedIterable {

  def apply[A](i: Iterable[A]): Iterable[A] = new Iterable[A] {
    def iterator = new Iterator[A] {
      val inner = i.iterator
      def hasNext = { synchronized { inner.hasNext } }
      def next = { synchronized { inner.next } }
    }
  }

}