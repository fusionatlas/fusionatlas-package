//package org.fusionatlas.enumerators
//import net.tqft.toolkit.amazon.S3
//
//object DeleteRecordsByPrefix {
//  def main(args: Array[String]): Unit = {
//    val bucket = S3("fusionatlas-extensions")
//
//    import net.tqft.toolkit.collections.Iterables._
//    bucket.keysWithPrefix("2.28826") consume { k =>
//      {
//        println("deleting " + k)
//        bucket -= k
//      }
//    }
//
//  }
//}
