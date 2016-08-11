//package net.tqft.toolkit
//
//import org.scalatest.FlatSpec
//import org.scalatest.matchers.ShouldMatchers
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
// 
//@RunWith(classOf[JUnitRunner])
//class ConcurrentTreeBuilderTest extends FlatSpec with ShouldMatchers {
//
//  "A ConcurrentTreeBuilder" should " not blow up!" in {
//	  val builder = ConcurrentTreeBuilder({ n: Int => { println(n); Some((n.toString, (1 to n/2).toList filter { n % _ == 0}))}})(List(124))
//	  builder.completedTree(_ => true).head.children.last.children.last.data should equal ("31")
//  }
//
//	
//}