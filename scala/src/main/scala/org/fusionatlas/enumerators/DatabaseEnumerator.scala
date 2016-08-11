package org.fusionatlas.enumerators

import org.fusionatlas.graphs._
import net.tqft.toolkit.Logging

trait EnumerationPersistence[G] {
  def lookup(d: Double, g: G): Option[(List[G], List[G])]
  def store(d: Double, g: G, result: (List[G], List[G]))
}

trait CachingEnumerator[G] extends Enumerator[G] with EnumerationPersistence[G] {

  abstract override def extend(d: Double, g: G, maximumGlobalDimensionIncrease: Option[Double] = None): (List[G], List[G]) = {
    // TODO filter lookup values when maximumGlobalDimensionIncrease is active?
    maximumGlobalDimensionIncrease match {
      case None => {
        lookup(d, g) match {
          case Some(r) => {
            Logging.info("found result in cache for: " + (d, g))
            r
          }
          case None =>
            val r = super.extend(d, g, None)
            store(d, g, r)
            r
        }
      }
      case Some(_) => super.extend(d, g, maximumGlobalDimensionIncrease)
    }
  }

}

object InMemoryPairEnumerator extends PairEnumerator with CachingEnumerator[PairOfBigraphsWithDuals] with InMemoryPersistence[PairOfBigraphsWithDuals]
object InMemoryFusionEnumerator extends FusionEnumerator with CachingEnumerator[FusionGraph] with InMemoryPersistence[FusionGraph]

object DatabasePairEnumerator extends PairEnumerator with CachingEnumerator[PairOfBigraphsWithDuals] with S3PairDatabase
object DatabaseFusionEnumerator extends FusionEnumerator with CachingEnumerator[FusionGraph] with S3FusionDatabase

trait NullPersistence[G] extends EnumerationPersistence[G] {
  override def lookup(d: Double, g: G): Option[(List[G], List[G])] = None
  override def store(d: Double, g: G, result: (List[G], List[G])) {}
}

trait InMemoryPersistence[G] extends EnumerationPersistence[G] {
  val cache = scala.collection.mutable.Map[(Double, G), (List[G], List[G])]()

  override def lookup(d: Double, g: G): Option[(List[G], List[G])] = {
    cache.get((d, g))
  }
  override def store(d: Double, g: G, result: (List[G], List[G])) {
    cache.put((d, g), result)
  }
}

trait ReadOnlyS3Database[G <: PersistentGraph[G]] extends NullPersistence[G] with Logging {
  protected def map: scala.collection.mutable.Map[(Double, G), (List[G], List[G])]
  override def lookup(d: Double, g: G) = {
    map.get((d, g))
  }
}

trait S3Database[G <: PersistentGraph[G]] extends ReadOnlyS3Database[G] {
  override def store(d: Double, g: G, result: (List[G], List[G])) {
    map += (((d, g), result))
  }
}

trait S3PairDatabase extends S3Database[PairOfBigraphsWithDuals] {
  val map = S3PairMaps.vinesAndWeeds
}
trait S3FusionDatabase extends S3Database[FusionGraph] {
  val map = S3FusionMaps.vinesAndWeeds
}

abstract class S3Maps[G] {
  import net.tqft.toolkit.amazon.S3
  import net.tqft.toolkit.collections.MapTransformer._
  import net.tqft.toolkit.Extractors.Double

  def bucket: String
  def buildGraph(s: String): G

  def parseValue(s: String): List[G] = s.split('|').toList filterNot (_ == "") map { buildGraph(_) }
  def buildValue(v: List[G]): String = v.mkString("|")
  def parseKey(keySuffix: String)(s: String): Option[(Double, G)] = {
    s.split("-").toList match {
      case t :: `keySuffix` :: Nil => {
        t.split("@").toList match {
          case Double(d) :: g :: Nil => Some(d, buildGraph(g))
          case _ => None
        }
      }
      case _ => None
    }
  }
  def buildKey(keySuffix: String)(p: (Double, G)): String = p._1 + "@" + p._2 + "-" + keySuffix

  lazy val vines = S3(bucket) transformValues (parseValue _, buildValue _) transformSomeKeys (parseKey("vines") _, buildKey("vines") _)
  lazy val weeds = S3(bucket) transformValues (parseValue _, buildValue _) transformSomeKeys (parseKey("weeds") _, buildKey("weeds") _)

  def vinesAndWeeds: scala.collection.mutable.Map[(Double, G), (List[G], List[G])] = new ZippedMutableMap(vines, weeds)

  private class ZippedMutableMap[A, B, C](m1: scala.collection.mutable.Map[A, B], m2: scala.collection.mutable.Map[A, C]) extends scala.collection.mutable.Map[A, (B, C)] {
    override def get(key: A): Option[(B, C)] = {
      for (x <- m1.get(key); y <- m2.get(key)) yield (x, y)
    }

    override def contains(key: A) = m1.contains(key)
    override def keys = m1.keys
    override def keySet = m1.keySet
    override def size = m1.size

    override def iterator: Iterator[(A, (B, C))] = {
      (m1.iterator zip m2.iterator) map { x => (x._1._1 ensuring { _ == x._2._1 }, (x._1._2, x._2._2)) }
    }

    override def +=(kv: (A, (B, C))) = {
      kv match {
        case (key, (b, c)) => {
          m1 += ((key, b))
          m2 += ((key, c))
          this
        }
      }
    }
    override def -=(key: A) = {
      m1 -= key
      m2 -= key
      this
    }
  }
}

object S3PairMaps extends S3Maps[PairOfBigraphsWithDuals] {
  def buildGraph(s: String) = PairOfBigraphsWithDuals(s)
  val bucket = "fusionatlas-extensions"
}

object S3FusionMaps extends S3Maps[FusionGraph] {
  def buildGraph(s: String) = FusionGraph(s)
  val bucket = "fusionatlas-fusion-extensions"
}

