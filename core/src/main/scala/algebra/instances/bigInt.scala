package algebra
package instances

import algebra.ring._
import cats.kernel.instances.BigIntOrder
import cats.kernel.{Hash, UnboundedEnumerable}

package object bigInt extends BigIntInstances

trait BigIntInstances extends cats.kernel.instances.BigIntInstances {
  private val instance: TruncatedDivision[BigInt] with CommutativeRing[BigInt] =
    new BigIntAlgebra

  implicit def bigIntAlgebra: CommutativeRing[BigInt] = instance

  implicit def bigIntTruncatedDivision: TruncatedDivision[BigInt] = instance
}

class BigIntAlgebra extends CommutativeRing[BigInt] with TruncatedDivision.forCommutativeRing[BigInt] with Serializable {

  override def compare(x: BigInt, y: BigInt): Int = x.compare(y)

  val zero: BigInt = BigInt(0)
  val one: BigInt = BigInt(1)

  def plus(a: BigInt, b: BigInt): BigInt = a + b
  def negate(a: BigInt): BigInt = -a
  override def minus(a: BigInt, b: BigInt): BigInt = a - b

  def times(a: BigInt, b: BigInt): BigInt = a * b

  override def pow(a: BigInt, k: Int): BigInt = a pow k

  override def fromInt(n: Int): BigInt = BigInt(n)
  override def fromBigInt(n: BigInt): BigInt = n

  def tquot(x: BigInt, y: BigInt): BigInt = x / y
  def tmod(x: BigInt, y: BigInt): BigInt = x % y
  override def tquotmod(x: BigInt, y: BigInt): (BigInt, BigInt) = x /% y
}
