/**
  * Copyright 2016 Martin Snyder
  *
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */
package com.martinsnyder.logger

case class SimpleWriter[T](log: List[String], value: T) {
  def map[B](f: T => B): SimpleWriter[B] =
    SimpleWriter(log, f(value))

  def flatMap[B](f: T => SimpleWriter[B]): SimpleWriter[B] = {
    val nextWriter = f(value)
    SimpleWriter(log ::: nextWriter.log, nextWriter.value)
  }
}
