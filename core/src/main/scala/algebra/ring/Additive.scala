package algebra
package ring

import scala.annotation.tailrec

trait AdditiveSemigroup[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def additive: Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = plus(x, y)
  }

  def plus(x: A, y: A): A

  def hasCommutativeAddition: Boolean = false

  def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else throw new IllegalArgumentException("Illegal non-positive exponent to sumN: %s" format n)

  protected[this] def positiveSumN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) plus(b, extra) else {
        val x = if ((k & 1) == 1) plus(b, extra) else extra
        loop(plus(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   * 
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def trySum(as: TraversableOnce[A]): Option[A] =
    as.reduceOption(plus)
}

trait AdditiveCommutativeSemigroup[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveSemigroup[A] {
  override def additive: CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
    def combine(x: A, y: A): A = plus(x, y)
  }
  override def hasCommutativeAddition: Boolean = true
}

trait AdditiveMonoid[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
  }

  def zero: A

  /**
    * Tests if `a` is zero.
    */
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)

  override def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else if (n == 0) zero
    else throw new IllegalArgumentException("Illegal negative exponent to sumN: %s" format n)

  /**
   * Given a sequence of `as`, compute the sum.
   */
  def sum(as: TraversableOnce[A]): A =
    as.foldLeft(zero)(plus)
}

trait AdditiveCommutativeMonoid[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveMonoid[A] with AdditiveCommutativeSemigroup[A] {
  override def additive: CommutativeMonoid[A] = new CommutativeMonoid[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveGroup[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
    override def remove(x: A, y: A): A = minus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))

  override def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else if (n == 0) zero
    else if (n == Int.MinValue) positiveSumN(negate(plus(a, a)), 1073741824)
    else positiveSumN(negate(a), -n)
}

trait AdditiveCommutativeGroup[@mb @sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveGroup[A] with AdditiveCommutativeMonoid[A] {
  override def additive: CommutativeGroup[A] = new CommutativeGroup[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
    override def remove(x: A, y: A): A = minus(x, y)
    def inverse(x: A): A = negate(x)
  }
}

trait AdditiveSemigroupFunctions {
  def plus[@mb @sp(Byte, Short, Int, Long, Float, Double) A](x: A, y: A)(implicit ev: AdditiveSemigroup[A]): A =
    ev.plus(x, y)

  def sumN[@mb @sp(Byte, Short, Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: AdditiveSemigroup[A]): A =
    ev.sumN(a, n)

  def trySum[@mb @sp(Byte, Short, Int, Long, Float, Double) A](as: TraversableOnce[A])(implicit ev: AdditiveSemigroup[A]): Option[A] =
    ev.trySum(as)
}

trait AdditiveMonoidFunctions extends AdditiveSemigroupFunctions {
  def zero[@mb @sp(Byte, Short, Int, Long, Float, Double) A](implicit ev: AdditiveMonoid[A]): A =
    ev.zero

  def isZero[@mb @sp(Byte, Short, Int, Long, Float, Double) A](a: A)(implicit ev0: AdditiveMonoid[A], ev1: Eq[A]): Boolean =
    ev0.isZero(a)

  def sum[@mb @sp(Byte, Short, Int, Long, Float, Double) A](as: TraversableOnce[A])(implicit ev: AdditiveMonoid[A]): A =
    ev.sum(as)
}

trait AdditiveGroupFunctions extends AdditiveMonoidFunctions {
  def negate[@mb @sp(Byte, Short, Int, Long, Float, Double) A](x: A)(implicit ev: AdditiveGroup[A]): A =
    ev.negate(x)
  def minus[@mb @sp(Byte, Short, Int, Long, Float, Double) A](x: A, y: A)(implicit ev: AdditiveGroup[A]): A =
    ev.minus(x, y)
}

object AdditiveSemigroup extends AdditiveSemigroupFunctions {
  @inline final def apply[A](implicit ev: AdditiveSemigroup[A]): AdditiveSemigroup[A] = ev
}
  
object AdditiveCommutativeSemigroup extends AdditiveSemigroupFunctions {
  @inline final def apply[A](implicit ev: AdditiveCommutativeSemigroup[A]): AdditiveCommutativeSemigroup[A] = ev
}
  
object AdditiveMonoid extends AdditiveMonoidFunctions {
  @inline final def apply[A](implicit ev: AdditiveMonoid[A]): AdditiveMonoid[A] = ev
}
  
object AdditiveCommutativeMonoid extends AdditiveMonoidFunctions {
  @inline final def apply[A](implicit ev: AdditiveCommutativeMonoid[A]): AdditiveCommutativeMonoid[A] = ev
}
  
object AdditiveGroup extends AdditiveGroupFunctions {
  @inline final def apply[A](implicit ev: AdditiveGroup[A]): AdditiveGroup[A] = ev
}
  
object AdditiveCommutativeGroup extends AdditiveGroupFunctions {
  @inline final def apply[A](implicit ev: AdditiveCommutativeGroup[A]): AdditiveCommutativeGroup[A] = ev
}
