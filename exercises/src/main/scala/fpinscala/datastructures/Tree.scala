package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

 // EXERCISE 25: Write a function size that counts the number of nodes (leaves and branches) in a tree.
 def size[A](t: Tree[A]) : Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
 } 

 // EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
 def maximum(t: Tree[Int]) : Int = t match {
    case Leaf(v) => v 
    case Branch(l,r) => maximum(l) max maximum(r)
 }

 // EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
 def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(_) => 0 
    case Branch(l,r) => 1 + (depth(l) max depth(r))
 }

 // EXERCISE 28: Write a function map, analogous to the method of the same name on List, 
 // that modifies each element in a tree with a given function.
 def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v)) 
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
 }

 // EXERCISE 29: Generalize size, maximum, depth, and map, writing a new
 // function fold that abstracts over their similarities.
def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
 }

 def sizeViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 1) (1 + _ + _)

 def maximumViaFold(t: Tree[Int]): Int =
  fold(t)(a => a) (_ max _)

 def depthViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 0) ((a: Int, b: Int) => 1 + (a max b))

 def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t) (a => Leaf(f(a)): Tree[B]) (Branch(_,_))

}
object TreeMain {

  import Tree._
  def main(args: Array[String]): Unit = {
  	val t = Branch(Branch(Leaf(1), Leaf(42)), Leaf(3))
  	println("size: " + size(t))
  	println("maximum: " + maximum(t))
  	println("depth: " + depth(t))
    println("map: " + map(t)(_ + 1))
    println("size via fold: " + sizeViaFold(t))
    println("maximum via fold: " + maximumViaFold(t))
    println("depth via fold: " + depthViaFold(t))
    println("map via fold: " + mapViaFold(t)(_ + 1))
  }

}