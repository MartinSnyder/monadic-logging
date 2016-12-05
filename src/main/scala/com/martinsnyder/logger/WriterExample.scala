/**
  * Copyright 2016 Martin Snyder
  *
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */
package com.martinsnyder.logger

object WriterExample {
  def loggedAdder(a: Int, b: Int): SimpleWriter[Int] =
    SimpleWriter(List(s"Added $a to $b"), a + b)

  def loggedToString(o: Any): SimpleWriter[String] =
    SimpleWriter(List(s"Converted $o to string"), o.toString)

  def main(args: Array[String]) = {
    val resultWithLog = for (
      sum <- loggedAdder(5, 7);
      converted <- loggedToString(sum)
    ) yield s"--> $converted <--"

    resultWithLog.log.foreach(l => println(s"LOG: $l"))
    println(s"Output is ${resultWithLog.value}")
  }
}
