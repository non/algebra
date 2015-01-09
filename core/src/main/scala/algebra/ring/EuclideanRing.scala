package algebra
package ring

import scala.annotation.tailrec

/**
 * EuclideanRing implements a Euclidean domain.
 * 
 * The formal definition says that every euclidean domain A has (at
 * least one) euclidean function f: A -> N (the natural numbers) where:
 * 
 *   (for every x and non-zero y) x = yq + r, and r = 0 or f(r) < f(y).
 * 
 * The idea is that f represents a measure of length (or absolute
 * value), and the previous equation represents finding the quotient
 * and remainder of x and y. So:
 * 
 *   quot(x, y) = q
 *   mod(x, y) = r
 * 
 * This type does not provide access to the Euclidean function, but
 * only provides the quot, mod, and quotmod operators.
 */
trait EuclideanRing[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with CommutativeRing[A] {
  def mod(a: A, b: A): A
  def quot(a: A, b: A): A
  def quotmod(a: A, b: A): (A, A) = (quot(a, b), mod(a, b))
}

trait EuclideanRingFunctions extends AdditiveGroupFunctions with MultiplicativeMonoidFunctions {
  def quot[@mb @sp(Byte, Short, Int, Long, Float, Double) A](x: A, y: A)(implicit ev: EuclideanRing[A]): A =
    ev.quot(x, y)
  def mod[@mb @sp(Byte, Short, Int, Long, Float, Double) A](x: A, y: A)(implicit ev: EuclideanRing[A]): A =
    ev.mod(x, y)
  def quotmod[@mb @sp(Byte, Short, Int, Long, Float, Double) A](x: A, y: A)(implicit ev: EuclideanRing[A]): (A, A) =
    ev.quotmod(x, y)
}

object EuclideanRing extends EuclideanRingFunctions {
  @inline final def apply[A](implicit ev: EuclideanRing[A]): EuclideanRing[A] = ev
}
