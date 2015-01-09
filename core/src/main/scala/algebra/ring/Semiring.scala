package algebra
package ring

import scala.annotation.tailrec

/**
 * Semiring consists of:
 * 
 *  - a commutative monoid for addition (+)
 *  - a semigroup for multiplication (*)
 * 
 * Alternately, a Semiring can be thought of as a ring without a
 * multiplicative identity or an additive inverse.
 *
 * A Semiring with an additive inverse (-) is a Rng.
 * A Semiring with a multiplicative identity (1) is a Rig.
 * A Semiring with both of those is a Ring.
 */
trait Semiring[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveCommutativeMonoid[A] with MultiplicativeSemigroup[A]

object Semiring extends AdditiveMonoidFunctions with MultiplicativeSemigroupFunctions {
  @inline final def apply[A](implicit ev: Semiring[A]): Semiring[A] = ev
}
