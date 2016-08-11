package net.tqft.toolkit.collections

trait Queue[A] {
  def enqueue(a: A)
  def enqueue(as: Traversable[A]) { for (a <- as) enqueue(a) }
  def dequeue: Option[A]
}
