package algebra.laws

import cats.kernel._
import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop._
import cats.kernel.instances.all._
import algebra.ring.{AdditiveCommutativeGroup, AdditiveCommutativeMonoid, GCDRing, Signed, TruncatedDivision}

object CombinationLaws {
  def apply[A: Eq: Arbitrary] = new CombinationLaws[A] {
    def Equ: Eq[A] = Eq[A]
    def Arb: Arbitrary[A] = implicitly[Arbitrary[A]]
  }
}

/**
 * Contains laws that are obeying by combination of types, for example
 * various kinds of signed rings.
 */
trait CombinationLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def signedAdditiveCommutativeMonoid(implicit signedA: Signed[A], A: AdditiveCommutativeMonoid[A]) = new DefaultRuleSet(
    name = "signedAdditiveCMonoid",
    parent = None,
    "ordered group" -> forAll { (x: A, y: A, z: A) =>
      signedA.lteqv(x, y) ==> signedA.lteqv(A.plus(x, z), A.plus(y, z))
    },
    "triangle inequality" -> forAll { (x: A, y: A) =>
      signedA.lteqv(signedA.abs(A.plus(x, y)), A.plus(signedA.abs(x), signedA.abs(y)))
    }
  )

  def signedAdditiveCommutativeGroup(implicit signedA: Signed[A], A: AdditiveCommutativeGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signedAdditiveCommutativeMonoid),
    "abs(x) equals abs(-x)" -> forAll { (x: A) =>
      signedA.abs(x) ?== signedA.abs(A.negate(x))
    }
  )

  // more a convention: as GCD is defined up to a unit, so up to a sign,
  // on an ordered GCD ring we require gcd(x, y) >= 0, which is the common
  // behavior of computer algebra systems
  def signedGCDRing(implicit signedA: Signed[A], A: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveCommutativeGroup),
    "gcd(x, y) >= 0" -> forAll { (x: A, y: A) =>
      signedA.isSignNonNegative(A.gcd(x, y))
    },
    "gcd(x, 0) === abs(x)" -> forAll { (x: A) =>
      A.gcd(x, A.zero) ?== signedA.abs(x)
    }
  )

}
