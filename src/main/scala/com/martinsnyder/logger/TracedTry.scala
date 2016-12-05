/**
  * Copyright 2016 Martin Snyder
  *
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */
package com.martinsnyder.logger

import scala.util.{ Failure, Success, Try }

object TracedTry {
  /**
    * The trace data that we store for each recorded entry
    *
    * @param eventTime The actual time that the event started (milliseconds from epoch, measured on the machine of execution)
    * @param operation The description of the operation performed
    * @param duration The number of milliseconds elapsed from operation start to finish
    * @param result Text description of the outcome of the operation or the failure itself
    */
  case class Entry(eventTime: Long, operation: String, arguments: Seq[AnyRef], duration: Long, result: Try[String])

  private def buildEntry[T]( f: => Try[T],
                             operation: Option[String] = None,
                             arguments: Seq[AnyRef] = Nil,
                             resultEmitter: T => String = (result: T) => ""
                           ): TracedTry[T] = {
    val start = System.currentTimeMillis()
    val result = f
    new TracedTry(List(Entry(start, operation.getOrElse(""), arguments, System.currentTimeMillis() - start, result map resultEmitter)), result)
  }

  /**
    * Creates a successful TracedResult for the Unit type
    *
    * @return TracedResult(Nil, Success)
    */
  def apply(): TracedTry[Unit] = new TracedTry[Unit](Nil, Success(()))

  /**
    * Lifts any value to a TracedResult with no trace data
    *
    * @param value some existing value
    * @tparam T the type of value
    * @return TracedResult(Nil, Result(value))
    */
  def apply[T](value: => T): TracedTry[T] =
    buildEntry(Try(value))

  /**
    * Times the execution of a function and pairs its result with a trace entry recording the execution time,
    * description, and outcome
    *
    * @param operation description of the operation being timed
    * @param f function to time
    * @param resultEmitter Optional function that describes a successful result (defaults to _.toString)
    * @tparam T return type of f
    * @return returns a TracedResult pairing the time entry for f along with the result for f
    */
  def time[T](operation: String, arguments: Seq[AnyRef] = Nil)(f: => T, resultEmitter: T => String = (result: T) => ""): TracedTry[T] = {
    buildEntry(Try(f), Some(operation), arguments, resultEmitter)
  }
}

/**
  * Monadic class that combines a result object with timing information designed for use in Scala for comprehensions.
  *
  * This object will accumulate timing information across calls (via flatMap) and will also short-circuit on failure
  * (via map/flatMap)
  *
  * @param trace timing entries associated with this result
  * @param value contained result.  Can be Success or Failure
  * @tparam T type of value (success only)
  */
class TracedTry[+T](val trace: List[TracedTry.Entry], val value: Try[T]) {
  def foreach[B](f: T => B): Unit = value.foreach(f)

  /**
    * Applies f to value, producing a new TracedResult of the new type.  Does nothing if value is a failure
    *
    * @param f transformation function to apply to value
    * @tparam B return type of f
    * @return a new TracedResult of the transformed type
    */
  def map[B](f: T => B): TracedTry[B] = {
    flatMap(v => TracedTry(f(v)))
  }

  /**
    * Applies f to value, producing a new TracedResult of the new type.  Does nothing if value is a failure.
    * This method will merge the timing elements produced by f with its own timing elements.
    *
    * @param f transformation function to apply to value
    * @tparam B return type of f
    * @return a new TracedResult of the transformed type containing merged elements
    */
  def flatMap[B](f: T => TracedTry[B]): TracedTry[B] = {
    value match {
      case Failure(e) =>
        // Failures can coerce to any type
        this.asInstanceOf[TracedTry[B]]

      case Success(v) =>
        val inner = f(v)
        new TracedTry(trace ::: inner.trace, inner.value)
    }
  }
}
