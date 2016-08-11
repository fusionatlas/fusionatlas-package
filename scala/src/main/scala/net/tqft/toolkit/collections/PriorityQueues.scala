package net.tqft.toolkit.collections

trait PriorityQueue[O, A] extends Queue[(O, A)] { self =>
  def orderBy(f: A => O): Queue[A] = new Queue[A] {
    def enqueue(a: A) = self.enqueue((f(a), a))
    def dequeue = self.dequeue.map(_._2)
  }
}
