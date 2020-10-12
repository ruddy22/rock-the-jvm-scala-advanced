package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean ) {

  /**
   * Exercise: Implement a functional set
   */

  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def isEmpty(): Boolean
  def head: A
  def tail: MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = NonEmptySet(elem, t = this) // t(tail) = this instance of EmptySet (aka new EmptySet)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def isEmpty: Boolean = true
  def head: A = throw new NoSuchElementException
  def tail: MySet[A] = throw new NoSuchElementException

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
}

case class NonEmptySet[A](h: A, t: MySet[A]) extends MySet[A] {
  /**
   * Daniels `contains` method implementation
   * def contains(elem: A): Boolean = elem == head || t.contains(elem)
   */
  def contains(elem: A): Boolean = {
    @tailrec
    def search(set: MySet[A]): Boolean =
      if (set.isEmpty) false
      else if (set.head == elem) true
      else search(set.tail)
    search(this)
  }
  def +(elem: A): NonEmptySet[A] = NonEmptySet(elem, this)
  def ++(anotherSet: MySet[A]): NonEmptySet[A] = NonEmptySet(h, t ++ anotherSet)
  def isEmpty: Boolean = false
  def head: A = h
  def tail: MySet[A] = t

  def map[B](f: A => B): NonEmptySet[B] = NonEmptySet(f(h), t.map(f))
  def flatMap[B](f: A => MySet[B]): MySet[B] = f(h) ++ t.flatMap(f)
  def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(h)) NonEmptySet(h, t.filter(predicate))
    else t.filter(predicate)
  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }
}

object MySetTest extends App {
  println(1)
  val emptySet = new EmptySet[Int]
  val mySet = NonEmptySet(1, NonEmptySet(2, emptySet))
  println("=== # contains ===")
  println(mySet.contains(2))
  println("=== # foreach ===")
  mySet + 3 foreach println
  println("=== # map ===")
  mySet + 3 map { _ + 1} foreach println
  println("=== # filter ===")
  mySet + 3 filter { _ % 2 == 0 } foreach println
}
