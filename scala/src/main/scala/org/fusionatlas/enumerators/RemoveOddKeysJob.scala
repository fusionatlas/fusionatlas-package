package org.fusionatlas.enumerators
import net.tqft.toolkit.amazon.S3

object RemoveOddKeysJob {
  def main(args : Array[String]) : Unit = {
    
    val bucket = S3("fusionatlas-extensions")
    val keys = bucket.keys.toList
    val vineKeys = keys.filter(_.endsWith("-vines"))
    val weedKeys = keys.filter(_.endsWith("-weeds"))
    
    println(vineKeys.size + " vines")
    println(weedKeys.size + " weeds")
    
    val vinesToDelete = vineKeys filterNot(weedKeys.map(_.dropRight(5) + "vines").contains)
    val weedsToDelete = weedKeys filterNot(vineKeys.map(_.dropRight(5) + "weeds").contains)
    
    println("deleting " + vinesToDelete.size + " vines")
    println("deleting " + weedsToDelete.size + " weeds")
    
    for(k <- vinesToDelete ::: weedsToDelete) {
      println("deleting " + k)
      bucket -= k
    }
  }
}
