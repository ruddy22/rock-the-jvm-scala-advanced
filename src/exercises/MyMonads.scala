package exercises

/**
 * My implementation
 */
//    trait Lazy[T] {
//      def flatMap[B](f: T => Lazy[B]): Lazy[B]
//    }
//
//    object Lazy {
//      def apply[T](param: => T): Lazy[T] = Lazi(param)
//    }
//
//    case class Lazi[T](param: T) extends Lazy[T] {
//      def flatMap[B](f: T => Lazy[B]): Lazy[B] = f(param)
//    }
/**
 * end of my implementation
 *
 * so I have a problem with my code
 * because `case class` doesn't support a `call-by-name` params
 * and ... param passed to `Lazy#apply` will be computed inside `apply` method =(
 */

/**
 * Daniels implementation
 */

class Lazy[+T](x: => T) {
  // call by need
  def flatMap[B](f: (=> T) => Lazy[B]): Lazy[B] = f(x)
  def use: T = x
}
object Lazy {
  def apply[T](x: => T): Lazy[T] = new Lazy[T](x)
}

/**
 * end of Daniels implementation
 */


object MyMonads extends App {
//  val codeBlock = {
//    println("this is side effect")
//    42
//  }

/**
  // every time print "this is side effect"
  println(codeBlock)
  println(codeBlock)

  println("======================================")

  // print "this is side effect" only once
  val lz = Lazi(codeBlock)
  println(lz.flatMap(x => Lazi(x + 1)))
  println(lz.flatMap(x => Lazi(x + 1)))

  // my code doesn't work correct
  // because "this is side effect" prints every time
*/

//  val lz = Lazi {
//    println("this is side effect")
//    42
//  }

//  val lz = Lazy(codeBlock)
  val lz = Lazy {
    println("this is side effect")
    42
  }

  val lz1 = lz.flatMap(x => Lazy(x + 1))
  val lz2 = lz.flatMap(x => Lazy(x + 1))
}
