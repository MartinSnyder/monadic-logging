// http://blog.tmorris.net/posts/the-writer-monad-using-scala-example/
package tonymorris

trait Monoid[A] {
  def append(a1: A, a2: A): A
  def empty: A
}

object Monoid {
  implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
    def empty = Nil
  }
}

case class Logger[LOG, A](log: LOG, value: A) {
  def map[B](f: A => B) =
    Logger(log, f(value))

  def flatMap[B](f: A => Logger[LOG, B])(implicit m: Monoid[LOG]) = {
    val x = f(value)
    Logger(m.append(log, x.log), x.value)
  }

  // insert much more
}

object Logger {
  def unital[LOG, A](value: A)(implicit m: Monoid[LOG]) =
    Logger(m.empty, value)

  // insert much more
}

object Util {
  // utility
  implicit def ListLogUtil[A](a: A) = new {
    def ~>[B](b: B) = Logger(List(a), b)

    def <|~[B](k: A => B) = Logger(List(k(a)), a)
  }

  def noLog[A](a: A) =
    Logger.unital[List[String], A](a)
}

// begin example

/*
$ scala Main 456
RESULT: 7000

LOG
---
adding one to 456
converting int to string 457
checking length of 457 for evenness
multiplying 1000 by 7 to produce 7000
*/
object Main {
  import Util._

  def main(args: Array[String]) {
    val x = args(0).toInt // parse int from command line

    val r =
      for(a <- addOne(x);
          b <- intString(a);
          c <- lengthIsEven(b);
          d <- noLog(hundredOrThousand(c));
          e <- times7(d)
      ) yield e

    println("RESULT: " + r.value)
    println
    println("LOG")
    println("---")
    r.log foreach println
  }

  def addOne(n: Int) =
    ("adding one to " + n) ~> (n + 1)

  def intString(n: Int) =
    ("converting int to string " + n) ~> n.toString

  def lengthIsEven(s: String) =
    ("checking length of " + s + " for evenness") ~> (s.length % 2 == 0)

  def hundredOrThousand(b: Boolean) = // no logging
    if(b) 100 else 1000

  def times7(n: Int) =
    (n * 7) <|~ ("multiplying " + n + " by 7 to produce " + _)
}