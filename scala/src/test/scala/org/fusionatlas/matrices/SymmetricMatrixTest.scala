package org.fusionatlas.matrices

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SymmetricMatrixTest extends FlatSpec with ShouldMatchers {
		
  "SymmetricMatrix" should "generate an array representing all entries" in {
	  new SymmetricMatrix(List(List(1), List(2,3))).asArray should equal (List(List(1,2), List(2,3)))
	  new SymmetricMatrix(List(List(0), List(0,0), List(3,0,0))).asArray should equal (List(List(0,0,3), List(0,0,0), List(3,0,0)))
  }

  "SymmetricMatrix" should "act as an odometer" in {
	  List(true, false, true).foldLeft(true)(_ && _) should equal (false)
  	  val sumAtMost3 = { s: SymmetricMatrix => 
  	  	(s.asArray map { row: List[Int] => (row.foldLeft(0)( _ + _ )) <= 3 }).foldLeft(true)(_ && _)
  	  }
  	  import net.tqft.toolkit.algebra.enumeration._
  	  Odometer(sumAtMost3)(new SymmetricMatrix(3)).toList.size should equal (336)
  }
  
  "SymmetricMatrx" should "complain if given an upper triangular matrix" in {
	  evaluating {
	 	  new SymmetricMatrix(List(List(1,1),List(1)))
	  } should produce [java.lang.AssertionError]
  }
  
  "SymmetricMatrix" should "report adjacencies" in {
	  val m = new SymmetricMatrix(List(List(1), List(1,1)))
	  m.neighbours(1) should equal (List(1,2))
	  m.neighbours(2) should equal (List(1,2))
  }
}