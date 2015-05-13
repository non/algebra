package algebra
package lattice

import scala.{specialized => sp}

import simulacrum._

/**
 * Boolean algebras are Heyting algebras with the additional
 * constraint that the law of the excluded middle is true
 * (equivalently, double-negation is true).
 * 
 * This means that in addition to the laws Heyting algebras obey,
 * boolean algebras also obey the following:
 * 
 *  - (a ∨ ¬a) = 1
 *  - ¬¬a = a
 * 
 * Boolean algebras generalize classical logic: one is equivalent to
 * "true" and zero is equivalent to "false". Boolean algebras provide
 * additional logical operators such as `xor`, `nand`, `nor`, and
 * `nxor` which are commonly used.
 * 
 * Every boolean algebras has a dual algebra, which involves reversing
 * true/false as well as and/or.
 */
@typeclass trait Bool[@sp(Int, Long) A] extends Any with Heyting[A] {
  override def imp(a: A, b: A): A = or(complement(a), b)

  def dual: Bool[A] = new DualBool(this)
}

class DualBool[@sp(Int, Long) A](orig: Bool[A]) extends Bool[A] {
  def one: A = orig.zero
  def zero: A = orig.one
  def and(a: A, b: A): A = orig.or(a, b)
  def or(a: A, b: A): A = orig.and(a, b)
  def complement(a: A): A = orig.complement(a)
  override def xor(a: A, b: A): A = orig.complement(orig.xor(a, b))

  override def imp(a: A, b: A): A = orig.and(orig.complement(a), b)
  override def nand(a: A, b: A): A = orig.nor(a, b)
  override def nor(a: A, b: A): A = orig.nand(a, b)
  override def nxor(a: A, b: A): A = orig.xor(a, b)

  override def dual: Bool[A] = orig
}

trait BoolFunctions {
  def dual[@sp(Int, Long) A](implicit ev: Bool[A]): Bool[A] =
    ev.dual
  def xor[@sp(Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.xor(x, y)
  def nand[@sp(Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.nand(x, y)
  def nor[@sp(Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.nor(x, y)
  def nxor[@sp(Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.nxor(x, y)
}

object Bool extends HeytingFunctions with BoolFunctions
