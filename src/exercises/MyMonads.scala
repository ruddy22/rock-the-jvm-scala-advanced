package exercises

trait Lazy[T] {
  def flatMap[B](f: T => Lazy[B]): Lazy[B]
}

object Lazy {
  def apply[T](param: => T): Lazy[T] = Lazi(param)
}

case class Lazi[T](param: T) extends Lazy[T] {
  def flatMap[B](f: T => Lazy[B]): Lazy[B] = f(param)
}


object MyMonads extends App {
  val codeBlock = {
    println("this is side effect")
    42
  }

  // every time print "this is side effect"
  println(codeBlock)
  println(codeBlock)

  println("======================================")

  // print "this is side effect" only once
  val lz = Lazi(codeBlock)
  println(lz.flatMap(x => Lazi(x + 1)))
  println(lz.flatMap(x => Lazi(x + 1)))
}
