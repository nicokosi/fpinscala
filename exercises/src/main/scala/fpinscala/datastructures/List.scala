package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
  }

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Nil => l
    case Cons(_, t) => Cons(h, t)
  }

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int =
    foldRight(l, 0) ((a: A, n: Int) => n + 1)
  
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h)) (f)
  }

  def sum_via_foldLeft(l: List[Int]) = 
    foldLeft(l, 0.0)(_ + _)
  
  def product_via_foldLeft(l: List[Double]) = 
    foldLeft(l, 1.0)(_ * _)

  def length_via_foldLeft[A] (l: List[A]) = 
    foldLeft(l, 0) ((n: Int, a: A) => n + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A]) ((a,b) => Cons(b,a))

  def append[A](l: List[A], a: A): List[A] =
    foldLeft(l, Nil:List[A]) ((b: List[A], a: A) => Cons(a,b))
    // or //foldRight(l, List(a)) ((x: A, y: List[A]) => Cons(x,y))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int]) ((x: Int, y: List[Int]) => Cons(x+1, y))

  def toStrings(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String]) ((x: Double, y: List[String]) => Cons(x.toString(), y))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B]) ((x: A, y: List[B]) => Cons(f(x), y))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A]) ((x: A, y: List[A]) => if (f(x)) y else Cons(x, y))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B]) ((x: A, y: List[B]) => append(f(x), y))

  def filter_via_flatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l) ((x: A) => if (f(x)) Nil:List[A] else List(x))

}
object Main {
  import List._
  def main(args: Array[String]): Unit = {
    println("tail: " + tail(List(41,42)))
    println("drop: " + drop(List(40,41,42), 2))
    println("dropWhile: " + dropWhile(List(40, 41, 42)) (_ < 42))
    println("setHead: " + setHead(List(1,2,3)) (42))
    println("length: " + length(List(1,2,3)))
    println("foldRight: " + foldRight(List(1,2,3), 0) (_-_))
    println("foldLeft: " + foldLeft(List(1,2,3), 0) (_-_))
    println("sum_via_foldLeft: " + sum_via_foldLeft(List(1,39)))
    println("product_via_foldLeft: " + product_via_foldLeft(List(2,21)))
    println("length_via_foldLeft: " + length_via_foldLeft(List(2,21)))
    println("reverse: " + reverse(List(1,2,3)))
    println("append: " + append(List(1,2), 3))
    println("addOne: " + addOne(List(1,2,3)))
    println("toStrings: " + toStrings(List(1,2,3)))
    println("map: " + map(List(1,2,3)) ((_ * 2.0)))
    println("filter: " + filter(List(1,2,3,4,5)) (_ % 2 != 0))
    println("flatMap: " + flatMap(List(1,2,3))(i => List(i,i)))
    println("filter_via_flatMap: " + filter_via_flatMap(List(1,2,3,4,5)) (_ % 2 != 0))
  }
}