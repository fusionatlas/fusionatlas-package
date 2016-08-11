package net.tqft.toolkit.amazon
import net.tqft.toolkit.collections.Stack
import net.tqft.toolkit.PrintLogging

object S3Stack extends PrintLogging {
  def apply(bucket: String, prefix: String = ""): Stack[String] = {
    val s3Bucket = S3(bucket)

    def random = String.format("%09d", int2Integer(new scala.util.Random().nextInt(1000000000)))
    def timestamp = (Long.MaxValue - System.currentTimeMillis()).toString + random
    val salt = new scala.util.Random().nextInt(1000)
    def chooseKey(keys: List[String]) = keys.sortBy(s => (s.dropRight(9).takeRight(3).reverse.toInt + salt) % 1000).headOption

    new Stack[String] {
      def push(s: String) = try {
        s3Bucket += ((prefix + timestamp, s))
      } catch {
        case e: Exception =>
          info("Caught exception while interacting with S3: ", e.getMessage())
          None
      }
      def pop = chooseKey(s3Bucket.keysWithPrefix(prefix, 20).take(20).toList).flatMap({ k =>
        try {
          val r = s3Bucket.get(k)
          if(r.nonEmpty) { s3Bucket -= k }
          r
        } catch {
          case e: Exception =>
            info("Caught exception while interacting with S3: ", e.getMessage())
            None
        }
      })
      def clear = for (k <- s3Bucket.keysWithPrefix(prefix)) {
        try {
          s3Bucket -= k
        } catch {
          case e: Exception => info("Caught exception while interacting with S3: ", e.getMessage())
        }
      }
    }
  }
}

