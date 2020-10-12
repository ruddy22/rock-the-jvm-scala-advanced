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
  def +(elem: A): NonEmptySet[A] =
    if (this contains elem) this
    else NonEmptySet(elem, this)

  /**
   * my implementation contains the error - missed existing element check
   * def ++(anotherSet: MySet[A]): NonEmptySet[A] = NonEmptySet(h, t ++ anotherSet)
   *
   * explanation:
   * [1 2 3] ++ [1 4 5] =
   * [2 3] ++ [1 4 5] + 1 =
   * [3] ++ [1 4 5] + 1 + 2 =
   * [] ++ [1 4 5] + 1 + 2 + 3 =
   * [1 4 5] + 1 + 2 + 3 =
   * [1 4 5] + 2 + 3 =
   * [1 4 5 2 3]
   */
  def ++(anotherSet: MySet[A]): MySet[A] = t ++ anotherSet + h
  def isEmpty: Boolean = false
  def head: A = h
  def tail: MySet[A] = t

  /**
   * Daniels implementation
   * def map[B](f: A => B): MySet[B] = t map f + f(h)
   */
  def map[B](f: A => B): NonEmptySet[B] = NonEmptySet(f(h), t.map(f))
  def flatMap[B](f: A => MySet[B]): MySet[B] = (t flatMap f) ++ f(h)

  /**
   * Daniels implementation
   *
   * def filter(predicate: A => Boolean): MySet[A] = {
   *   val filteredTail = tail filter predicate
   *   if (predicate(h)) filteredTail + h
   *   else filteredTail
   * }
   */
  def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(h)) NonEmptySet(h, t.filter(predicate))
    else t.filter(predicate)
  def foreach(f: A => Unit): Unit = {
    f(h)
    t foreach f
  }
}

object MySet {
  /**
   * explanation
   * val s = MySet(1,2,3,3) =
   * buildSet(seq(1,2,3,3), []) =
   * buildSet(seq(2,3,3), [] + 1) =
   * buildSet(seq(3,3), [1] + 2) =
   * buildSet(seq(3), [1, 2] + 3) =
   * buildSet(seq(), [1, 2, 3] + 3) =
   * buildSet(seq(), [1, 2, 3]) =
   * [1,2,3]
   */
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values, new EmptySet[A])
  }
}

object MySetTest extends App {
  println(1)
  val mySet = MySet(1,2,8)
  println("=== # contains ===")
  println(mySet.contains(2))
  println("=== # foreach ===")
  mySet + 3 foreach println
  println("=== # map ===")
  mySet + 3 map { _ + 1} foreach println
  println("=== # filter ===")
  mySet + 3 filter { _ % 2 == 0 } foreach println
  println("=== # ++ ===")
  mySet ++ MySet(1, 2, 4) foreach println
  println("=== # flatMap ===")
  mySet + 3 flatMap { el => MySet(el, el * 10) } foreach println
  println("complex example")
  MySet(1,2,3) + 3 ++ MySet(-1, -2) map(x => x + 2) flatMap(x => MySet(x, x * 10)) filter (_ > 20) foreach println
}
