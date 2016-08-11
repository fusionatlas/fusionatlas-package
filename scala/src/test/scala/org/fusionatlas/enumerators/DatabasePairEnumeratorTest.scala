package org.fusionatlas.enumerators

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.fusionatlas.graphs._

import scala.math._

@RunWith(classOf[JUnitRunner])
class DatabasePairEnumeratorTest extends FlatSpec with ShouldMatchers {
  
  "DatabasePairEnumerator" should "not crash on initialization" in {
    println(DatabasePairEnumerator.toString)
    println(DatabasePairEnumerator.hashCode)
  }
}