package algebra.laws

import algebra.lattice.{BoundedMeetSemilattice, Lattice}

trait BoundedLatticeLaws[A] extends LatticeLaws[A] with BoundedMeetLatticeLaws[A] with BoundedJoinLatticeLaws[A] {
  override implicit def S: BoundedLattice[A]
}

object BoundedLatticeLaws {
  def apply[A](implicit ev: BoundedLattice[A]): BoundedLatticeLaws[A] =
    new BoundedLatticeLaws[A] { def S: BoundedLattice[A] = ev }
}

