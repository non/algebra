package algebra

import scala.{ specialized => sp }

import simulacrum._

/**
 * CommutativeMonoid represents a commutative monoid.
 * 
 * A monoid is commutative if for all x and y, x |+| y === y |+| x.
 */
@typeclass trait CommutativeMonoid[@sp(Int, Long, Float, Double) A] extends Any with Monoid[A] with CommutativeSemigroup[A]

object CommutativeMonoid extends MonoidFunctions {

  /**
   * This method converts an additive instance into a generic
   * instance.
   * 
   * Given an implicit `AdditiveCommutativeMonoid[A]`, this method
   * returns a `CommutativeMonoid[A]`.
   */
  @inline final def additive[A](implicit ev: ring.AdditiveCommutativeMonoid[A]): CommutativeMonoid[A] =
    ev.additive

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   * 
   * Given an implicit `MultiplicativeCommutativeMonoid[A]`, this
   * method returns a `CommutativeMonoid[A]`.
   */
  @inline final def multiplicative[A](implicit ev: ring.MultiplicativeCommutativeMonoid[A]): CommutativeMonoid[A] =
    ev.multiplicative
}
