val lst = List(1,2,3,4)

val adderFn: Int => Int => Int = x => y => x + y
val add1 = adderFn(1)
println(add1(2))

println(lst map add1)

def adderM(x: Int)(y: Int): Int = x + y
val add2 = adderM(2) _
println(add2(2))

println(lst map add2)

def inc(x: Int): Int = x + 1
println(lst map inc)

def sumOf3(x: Int, y: Int, z: Int): Int = x + y + z
def curriedSum: Int => Int = sumOf3(1, _:Int, 3)
println(curriedSum(2))

// ===================================

def byName(n: => Int): Int = n + 1
def byFunction(f: () => Int): Int = f() + 1

// Int
//println(byName(1))
//println(byFunction(() => 1))

// method
def method: Int = 42
//println(byName(method))
//println(byFunction(method _)) // deprecated

// parenMethod
def parenMethod(): Int = 42
//println(byName(parenMethod()))
//println(byFunction(parenMethod))

val plus1: Int => Int = n => n + 1
//byName(plus1)

lazy val x: Int = {
  println("hello")
  42
}
println(x)
println(x)

def byNameMethod(n: => Int): Int = {
  // CALL BY NEED
  lazy val t = n // only evaluated once
  t + t + t + 1
}
def retrieveMagicValue = {
  // side effect or a long computation
  println("waiting")
  Thread.sleep(1000)
  42
}

println(byNameMethod(retrieveMagicValue))

def fn(n: Int): Unit = 1 to n foreach { _: Int => println("hello world") }
fn(5)

def procLst(n: Int, arr: List[Int]): List[Int] =
  arr.flatMap(x => List.fill(n)(x))

println(procLst(3, List(1,2,3,4)))

val fibs:LazyList[Int] = 0 #:: 1 #:: (fibs zip fibs.tail).map{ t => t._1 + t._2 }
println(fibs.iterator.next)


//case class Monad[T](x: T) {
//  def flatMap[B](f: T => Monad[B]): Monad[B] = f(x)
//  def use: T = x
//  def map[B](f: T => B): Monad[B] = Monad(f(x))
//  def flatten(m: Monad[Monad[T]]): Monad[T] = m.use
//}
//
//object Monad {
//  def apply[T](x: T): Monad[T] = Monad(x)
//}
//
//val mappedMonadValue = Monad(1).flatMap(x => Monad(x + 1)).map(x => x + 1)
//println(mappedMonadValue)

/**
 * apparently, I misunderstood the task ...
 * it turned out that it was necessary to implement `map` and` flatten` in terms of `flatMp`
 */

case class Monad[T](x: T) {
  def flatMap[B](f: T => Monad[B]): Monad[B] = f(x)
  def map[B](f: T => B): Monad[B] = flatMap(x => Monad(f(x)))
  def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap(x => x)
}
object Monad {
  def apply[T](x: T): Monad[T] = Monad(x)
}

