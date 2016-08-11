package net.tqft.toolkit

trait PrintLogging {
  private def now = new java.util.Date().toString
  private def thread = Thread.currentThread.getName
  
  def trace(msg: => Any) = println(now + " " + thread + " " + msg)
  def debug(msg: => Any) = println(now + " " + thread + " " + msg)
  def info(msg: => Any) = println(now + " " + thread + " " + msg)
  def warn(msg: => Any) = println(now + " " + thread + " " + msg)
  def error(msg: => Any, e:Throwable) = println(now + " " + thread + " " + msg,e)
  def fatal(msg: => Any, e:Throwable) = println(now + " " + thread + " " + msg,e)
}

object PrintLogging extends PrintLogging