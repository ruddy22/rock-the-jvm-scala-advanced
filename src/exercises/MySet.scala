package exercises

trait MySet[A] extends (A => Boolean ) {

  /**
   * Exercise: Implement a functional set
   */

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
}
