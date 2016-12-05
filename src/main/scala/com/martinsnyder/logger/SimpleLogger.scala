/**
  * Copyright 2016 Martin Snyder
  *
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */
package com.martinsnyder.logger

object SimpleLogger {
  case class Entry(msg: String, timestamp: Long = System.currentTimeMillis())

  def lift[T](value: T): SimpleLogger[T] = SimpleLogger(Nil, value)
}

case class SimpleLogger[T](log: List[SimpleLogger.Entry], value: T) {
  def map[B](f: T => B): SimpleLogger[B] =
    SimpleLogger(log, f(value))

  def flatMap[B](f: T => SimpleLogger[B]): SimpleLogger[B] = {
    val nextWriter = f(value)
    SimpleLogger(log ::: nextWriter.log, nextWriter.value)
  }
}
