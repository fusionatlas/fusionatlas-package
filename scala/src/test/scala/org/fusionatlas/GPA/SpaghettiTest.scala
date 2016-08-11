package org.fusionatlas.GPA

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class SpaghettiTest extends FlatSpec with ShouldMatchers {

  "reverse" should "be an involution" in {
    val s = Spaghetti[NoLabel](NoLabel, 4, 2, 5, 3, initialTangentDirection = 1, 2)
    s.reverse should equal (s)
    s.reverse.toString should not equal (s.toString)
    s.reverse.reverse.toString should equal (s.toString)
  }

}

