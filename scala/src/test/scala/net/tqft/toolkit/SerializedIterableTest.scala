package net.tqft.toolkit

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
 
@RunWith(classOf[JUnitRunner])
class SerializedSoftReferenceIterableTest extends FlatSpec with ShouldMatchers {

  "A SerializedSoftReferenceIterable" should "behave exactly like the underlying Iterable" in {
	  new SerializedSoftReferenceIterable(1 to 10, this.getClass.getClassLoader).toList should equal ((1 to 10).toList)
  }

	
}