package fpinscala.laziness

import Stream._

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def toList: List[A] =
    foldRight(List[A]())((a, b) => a::b)

  def take(n: Int): Stream[A] = uncons match {
    case Some((h, t)) => if (n > 0) cons(h,  t.take(n-1)) 
                         else empty
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((h, t)) => if (p(h)) cons(h,  t.takeWhile(p)) 
                         else empty
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty:Stream[A])((a, b) => if (p(a)) cons(a, b)
                                         else empty)
  }

}
object Stream {
  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")

}
object StreamMain {

  def main(args: Array[String]): Unit = {
    def stream123 = Stream(1,2,3)
    println("toList (empty stream): " + empty.toList)
    println("toList (3-int stream): " + stream123.toList)
    println()

    println("take 2nd (empty stream): " + empty.take(2).toList)
    println("take 2nd (1-2-3 stream): " + stream123.take(2).toList)
    println()

    println("take while < 3 (1-2-3 stream): " + stream123.takeWhile(_ < 3).toList)
    println()

    println("for all < 4 (1-2-3 stream): " + stream123.forAll(_ < 4))
    println("for all < 2 (1-2-3 stream): " + stream123.forAll(_ < 2))
    def lessThanWithSysout(a: Int, b: Int) = {
      print(".")
      a < b
    }
    println("for all < 2 (1-2-3 stream) + sysout: " + stream123.forAll(lessThanWithSysout(_, 2)))
    println()

    def stream1230 = Stream(1,2,3,0)
    println("take while < 3 (1-2-3-0 stream) using foldRight: " + stream1230.takeWhileUsingFoldRight(_ < 3).toList)
    println()
  }

}