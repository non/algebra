package algebra
package laws

import algebra.ring._

trait RigLaws[A] extends SemiringLaws[A] with MultiplicativeMonoidLaws[A] {
  override implicit def S: Rig[A]
}

object RigLaws {
  def apply[A](implicit ev: Rig[A]): RigLaws[A] =
    new RigLaws[A] { def S: Rig[A] = ev }
}
