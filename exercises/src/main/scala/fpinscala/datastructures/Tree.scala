package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

 // EXERCISE 25: Write a function size that counts the number of nodes (leaves and branches) in a tree.
 def size[A](t: Tree[A]) : Int = t match {
    case Leaf(v) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
 } 

 // EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
 def maximum(t: Tree[Int]) : Int = t match {
    case Leaf(v) => v 
    case Branch(l,r) => maximum(l) max maximum(r)
 }

 // EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
 def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(v) => 0 
    case Branch(l,r) => 1 + (depth(l) max depth(r))
 }

 // EXERCISE 28: Write a function map, analogous to the method of the same name on List, 
 // that modifies each element in a tree with a given function.
 def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v)) 
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
 }

}

object TreeMain {

  import Tree._
  def main(args: Array[String]): Unit = {
  	val t = Branch(Branch(Leaf(1), Leaf(42)), Leaf(3))
  	println("size: " + size(t))
  	println("maximum: " + maximum(t))
  	println("depth: " + depth(t))
    println("map: " + map(t) (_ + 1))
  }

}