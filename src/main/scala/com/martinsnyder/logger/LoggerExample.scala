/**
  * Copyright 2016 Martin Snyder
  *
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */
package com.martinsnyder.logger

import com.martinsnyder.logger.SimpleLogger.Entry

object LoggerExample {
  def loggedAdder(a: Int, b: Int): SimpleLogger[Int] =
    SimpleLogger(List(Entry(s"Added $a to $b")), a + b)

  def loggedToString(o: Any): SimpleLogger[String] =
    SimpleLogger(List(Entry(s"Converted $o to string")), o.toString)

  def main(args: Array[String]) = {
    val resultWithLog = for (
      sum <- loggedAdder(5, 7);
      _ <- SimpleLogger.lift(Thread.sleep(15));
      converted <- loggedToString(sum)
    ) yield s"--> $converted <--"

    resultWithLog.log.foreach(l => println(s"LOG: $l"))
    println(s"Output is ${resultWithLog.value}")
  }
}
