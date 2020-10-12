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
  def contains(elem: A): Boolean = {
    @tailrec
    def search(set: MySet[A]): Boolean =
      if (set.isEmpty) false
      else if (set.head == elem) true
      else search(set.tail)
    search(this)
  }
  def +(elem: A): NonEmptySet[A] = ???
  def ++(anotherSet: MySet[A]): NonEmptySet[A] = ???
  def isEmpty: Boolean = false
  def head: A = h
  def tail: MySet[A] = t

  def map[B](f: A => B): NonEmptySet[B] = ???
  def flatMap[B](f: A => MySet[B]): NonEmptySet[B] = ???
  def filter(predicate: A => Boolean): MySet[A] = ???
  def foreach(f: A => Unit): Unit = ???
}

object MySetTest extends App {
  println(1)
  val emptySet = new EmptySet[Int]
  val mySet = NonEmptySet(1, NonEmptySet(2, emptySet))
  println(mySet.contains(2))
}
