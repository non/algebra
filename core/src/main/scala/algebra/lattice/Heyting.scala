package algebra
package lattice

import ring.CommutativeRing

/**
 * Heyting algebras are bounded lattices that are also equipped with
 * an additional binary operation `imp` (for impliciation, also
 * written as →).
 * 
 * Implication obeys the following laws:
 * 
 *  - a → a = 1
 *  - a ∧ (a → b) = a ∧ b
 *  - b ∧ (a → b) = b
 *  - a → (b ∧ c) = (a → b) ∧ (a → c)
 * 
 * In heyting algebras, `and` is equivalent to `meet` and `or` is
 * equivalent to `join`; both methods are available.
 * 
 * Heyting algebra also define `complement` operation (sometimes
 * written as ¬a). The complement of `a` is equivalent to `(a → 0)`,
 * and the following laws hold:
 * 
 *  - a ∧ ¬a = 0
 * 
 * However, in Heyting algebras this operation is only a
 * pseudo-complement, since Heyting algebras do not necessarily
 * provide the law of the excluded middle. This means that there is no
 * guarantee that (a ∨ ¬a) = 1.
 * 
 * Heyting algebras model intuitionistic logic. For a model of
 * classical logic, see the boolean algebra type class implemented as
 * `Bool`.
 */
trait Heyting[@mb @sp(Boolean, Byte, Short, Int, Long) A] extends Any with BoundedLattice[A] { self =>
  def and(a: A, b: A): A
  def meet(a: A, b: A): A = and(a, b)

  def or(a: A, b: A): A
  def join(a: A, b: A): A = or(a, b)

  def imp(a: A, b: A): A
  def complement(a: A): A

  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A = complement(or(a, b))
  def nxor(a: A, b: A): A = complement(xor(a, b))

  def asCommutativeRing: CommutativeRing[A] =
    new CommutativeRing[A] {
      def zero: A = self.zero
      def one: A = self.one
      def plus(x: A, y: A): A = self.xor(x, y)
      def negate(x: A): A = self.complement(x)
      def times(x: A, y: A): A = self.and(x, y)
    }
}

trait HeytingFunctions {
  def zero[@mb @sp(Boolean, Byte, Short, Int, Long) A](implicit ev: Bool[A]): A = ev.zero
  def one[@mb @sp(Boolean, Byte, Short, Int, Long) A](implicit ev: Bool[A]): A = ev.one

  def complement[@mb @sp(Boolean, Byte, Short, Int, Long) A](x: A)(implicit ev: Bool[A]): A =
    ev.complement(x)

  def and[@mb @sp(Boolean, Byte, Short, Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.and(x, y)
  def or[@mb @sp(Boolean, Byte, Short, Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.or(x, y)
  def imp[@mb @sp(Boolean, Byte, Short, Int, Long) A](x: A, y: A)(implicit ev: Bool[A]): A =
    ev.imp(x, y)
}


object Heyting extends HeytingFunctions {

  /**
   * Access an implicit `Heyting[A]`.
   */
  @inline final def apply[@mb @sp(Boolean, Byte, Short, Int, Long) A](implicit ev: Heyting[A]): Heyting[A] = ev
}
