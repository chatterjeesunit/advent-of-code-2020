package day07

case class Bag (color: String, count:Int) {

  override def equals(obj: Any): Boolean = obj match {
    case otherBag: Bag => otherBag.color.equals(color)
    case _ => false
  }

  override def hashCode(): Int = color.hashCode
}
