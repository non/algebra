package algebra
package std

package object string extends StringInstances

trait StringInstances {
  implicit val stringOrder: Order[String] = new StringOrder
  implicit val stringMonoid = new StringMonoid
}

class StringOrder extends Order[String] {
  def compare(x: String, y: String): Int = x compare y
}

class StringMonoid extends Monoid[String] {
  def neutral: String = ""
  def combine(x: String, y: String): String = x + y

  override def combineAll(xs: TraversableOnce[String]): String = {
    val sb = new StringBuilder
    xs.foreach(sb.append)
    sb.toString
  }
}
