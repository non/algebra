package algebra
package laws

import algebra.ring._

trait RingLaws[A] extends RigLaws[A] with RngLaws[A] {
  override implicit def S: Ring[A]
}

object RingLaws {
  def apply[A](implicit ev: Ring[A]): RingLaws[A] =
    new RingLaws[A] { def S: Ring[A] = ev }
}
/*

import catalysts.Platform
import cats.kernel.laws._

import org.typelevel.discipline.Predicate

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object RingLaws {
  def apply[A : Eq : Arbitrary: AdditiveMonoid]: RingLaws[A] =
    withPred[A](new Predicate[A] {
      def apply(a: A): Boolean = Eq[A].neqv(a, AdditiveMonoid[A].zero)
    })

  def withPred[A: Eq: Arbitrary](pred0: Predicate[A]): RingLaws[A] = new RingLaws[A] {
    def Arb = implicitly[Arbitrary[A]]
    def pred = pred0
    val nonZeroLaws = new GroupLaws[A] {
      def Arb = Arbitrary(arbitrary[A] filter pred)
      def Equ = Eq[A]
    }
  }
}

trait RingLaws[A] extends GroupLaws[A] { self =>

  // must be a val (stable identifier)
  val nonZeroLaws: GroupLaws[A]
  def pred: Predicate[A]

  def withPred(pred0: Predicate[A], replace: Boolean = true): RingLaws[A] =
    RingLaws.withPred(if (replace) pred0 else pred && pred0)(Equ, Arb)

  def setNonZeroParents(props: nonZeroLaws.GroupProperties, parents: Seq[nonZeroLaws.GroupProperties]): nonZeroLaws.GroupProperties =
    new nonZeroLaws.GroupProperties(
      name = props.name,
      parents = parents,
      props = props.props: _*
    )

  implicit def Arb: Arbitrary[A]
  implicit def Equ: Eq[A] = nonZeroLaws.Equ

  // additive groups

  def additiveSemigroup(implicit A: AdditiveSemigroup[A]) = new AdditiveProperties(
    base = semigroup(A.additive),
    parents = Nil,
    Rules.serializable(A),
    Rules.repeat1("sumN")(A.sumN),
    Rules.repeat2("sumN", "+")(A.sumN)(A.plus)
  )

  def additiveCommutativeSemigroup(implicit A: AdditiveCommutativeSemigroup[A]) = new AdditiveProperties(
    base = commutativeSemigroup(A.additive),
    parents = List(additiveSemigroup)
  )

  def additiveMonoid(implicit A: AdditiveMonoid[A]) = new AdditiveProperties(
    base = monoid(A.additive),
    parents = List(additiveSemigroup),
    Rules.repeat0("sumN", "zero", A.zero)(A.sumN),
    Rules.collect0("sum", "zero", A.zero)(A.sum)
  )

  def additiveCommutativeMonoid(implicit A: AdditiveCommutativeMonoid[A]) = new AdditiveProperties(
    base = commutativeMonoid(A.additive),
    parents = List(additiveMonoid)
  )

  def additiveGroup(implicit A: AdditiveGroup[A]) = new AdditiveProperties(
    base = group(A.additive),
    parents = List(additiveMonoid),
    Rules.consistentInverse("subtract")(A.minus)(A.plus)(A.negate)
  )

  def additiveCommutativeGroup(implicit A: AdditiveCommutativeGroup[A]) = new AdditiveProperties(
    base = commutativeGroup(A.additive),
    parents = List(additiveGroup)
  )

  // multiplicative groups

  def multiplicativeSemigroup(implicit A: MultiplicativeSemigroup[A]) = new MultiplicativeProperties(
    base = semigroup(A.multiplicative),
    nonZeroBase = None,
    parent = None,
    Rules.serializable(A),
    Rules.repeat1("pow")(A.pow),
    Rules.repeat2("pow", "*")(A.pow)(A.times)
  )

  def multiplicativeCommutativeSemigroup(implicit A: MultiplicativeCommutativeSemigroup[A]) = new MultiplicativeProperties(
    base = semigroup(A.multiplicative),
    nonZeroBase = None,
    parent = Some(multiplicativeSemigroup)
  )

  def multiplicativeMonoid(implicit A: MultiplicativeMonoid[A]) = new MultiplicativeProperties(
    base = monoid(A.multiplicative),
    nonZeroBase = None,
    parent = Some(multiplicativeSemigroup),
    Rules.repeat0("pow", "one", A.one)(A.pow),
    Rules.collect0("product", "one", A.one)(A.product)
  )

  def multiplicativeCommutativeMonoid(implicit A: MultiplicativeCommutativeMonoid[A]) = new MultiplicativeProperties(
    base = commutativeMonoid(A.multiplicative),
    nonZeroBase = None,
    parent = Some(multiplicativeMonoid)
  )

  def multiplicativeGroup(implicit A: MultiplicativeGroup[A]) = new MultiplicativeProperties(
    base = monoid(A.multiplicative),
    nonZeroBase = Some(setNonZeroParents(nonZeroLaws.group(A.multiplicative), Nil)),
    parent = Some(multiplicativeMonoid),
    // pred is used to ensure y is not zero.
    "consistent division" -> forAll { (x: A, y: A) =>
      pred(y) ==> (A.div(x, y) ?== A.times(x, A.reciprocal(y)))
    }
  )

  def multiplicativeCommutativeGroup(implicit A: MultiplicativeCommutativeGroup[A]) = new MultiplicativeProperties(
    base = commutativeMonoid(A.multiplicative),
    nonZeroBase = Some(setNonZeroParents(nonZeroLaws.commutativeGroup(A.multiplicative), multiplicativeGroup.nonZeroBase.toSeq)),
    parent = Some(multiplicativeGroup)
  )

  // rings

  def semiring(implicit A: Semiring[A]) = new RingProperties(
    name = "semiring",
    al = additiveCommutativeMonoid,
    ml = multiplicativeSemigroup,
    parents = Seq.empty,
    Rules.distributive(A.plus)(A.times)
  )

  def rng(implicit A: Rng[A]) = new RingProperties(
    name = "rng",
    al = additiveCommutativeGroup,
    ml = multiplicativeSemigroup,
    parents = Seq(semiring)
  )

  def rig(implicit A: Rig[A]) = new RingProperties(
    name = "rig",
    al = additiveCommutativeMonoid,
    ml = multiplicativeMonoid,
    parents = Seq(semiring)
  )

  def ring(implicit A: Ring[A]) = new RingProperties(
    // TODO fromParents
    name = "ring",
    al = additiveCommutativeGroup,
    ml = multiplicativeMonoid,
    parents = Seq(rig, rng),
    "fromInt" -> forAll { (n: Int) =>
      Ring.fromInt[A](n) ?== A.sumN(A.one, n)
    },
    "fromBigInt" -> forAll { (ns: List[Int]) =>
      val actual = Ring.fromBigInt[A](ns.map(BigInt(_)).foldLeft(BigInt(1))(_ * _))
      val expected = ns.map(A.fromInt).foldLeft(A.one)(A.times)
      actual ?== expected
    }
  )

  // commutative rings

  def commutativeSemiring(implicit A: CommutativeSemiring[A]) = new RingProperties(
    name = "commutativeSemiring",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeSemigroup,
    parents = Seq(semiring)
  )

  def commutativeRng(implicit A: CommutativeRng[A]) = new RingProperties(
    name = "commutativeRng",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeSemigroup,
    parents = Seq(rng, commutativeSemiring)
  )

  def commutativeRig(implicit A: CommutativeRig[A]) = new RingProperties(
    name = "commutativeRig",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeMonoid,
    parents = Seq(rig, commutativeSemiring)
  )

  def commutativeRing(implicit A: CommutativeRing[A]) = new RingProperties(
    name = "commutative ring",
    al = additiveCommutativeGroup,
    ml = multiplicativeCommutativeMonoid,
    parents = Seq(ring, commutativeRig, commutativeRng)
  )

  // boolean rings

  def boolRng(implicit A: BoolRng[A]) = RingProperties.fromParent(
    name = "boolean rng",
    parent = commutativeRng,
    Rules.idempotence(A.times)
  )

  def boolRing(implicit A: BoolRing[A]) = RingProperties.fromParent(
    name = "boolean ring",
    parent = commutativeRing,
    Rules.idempotence(A.times)
  )

  // Everything below fields (e.g. rings) does not require their multiplication
  // operation to be a group. Hence, we do not check for the existence of an
  // inverse. On the other hand, fields require their multiplication to be an
  // abelian group. Now we have to worry about zero.
  //
  // The usual text book definition says: Fields consist of two abelian groups
  // (set, +, zero) and (set \ zero, *, one). We do the same thing here.
  // However, since law checking for the multiplication does not include zero
  // any more, it is not immediately clear that desired properties like
  // zero * x == x * zero hold.
  // Luckily, these follow from the other field and group axioms.
  def field(implicit A: Field[A]) = new RingProperties(
    name = "field",
    al = additiveCommutativeGroup,
    ml = multiplicativeCommutativeGroup,
    parents = Seq(commutativeRing),
    "fromDouble" -> forAll { (n: Double) =>
      if (Platform.isJvm) {
        // TODO: BigDecimal(n) is busted in scalajs, so we skip this test.
        val bd = new java.math.BigDecimal(n)
        val unscaledValue = new BigInt(bd.unscaledValue)
        val expected =
          if (bd.scale > 0) {
            A.div(A.fromBigInt(unscaledValue), A.fromBigInt(BigInt(10).pow(bd.scale)))
          } else {
            A.fromBigInt(unscaledValue * BigInt(10).pow(-bd.scale))
          }
        Field.fromDouble[A](n) ?== expected
      } else {
        Prop(true)
      }
    }
  )

  // property classes

  class AdditiveProperties(
    val base: GroupLaws[A]#GroupProperties,
    val parents: Seq[AdditiveProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val name = "additive " + base.name
    val bases = List("base" -> base)
  }

  class MultiplicativeProperties(
    val base: GroupLaws[A]#GroupProperties,
    val nonZeroBase: Option[nonZeroLaws.GroupProperties],
    val parent: Option[MultiplicativeProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val name = "multiplicative " + base.name
    val bases = Seq("base" -> base) ++ nonZeroBase.map("non-zero base" -> _)
  }

  object RingProperties {
    def fromParent(name: String, parent: RingProperties, props: (String, Prop)*) =
      new RingProperties(name, parent.al, parent.ml, Seq(parent), props: _*)
  }

  class RingProperties(
    val name: String,
    val al: AdditiveProperties,
    val ml: MultiplicativeProperties,
    val parents: Seq[RingProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Seq("additive" -> al, "multiplicative" -> ml)
  }
}
*/
