package day17

case class HyperCoordinate(x: Int, y:Int, z:Int, f: Int) {
  override def equals(obj: Any): Boolean = obj match {
    case c: HyperCoordinate => x==c.x && y==c.y && z==c.z && f==c.f
    case _ => false
  }

  override def hashCode(): Int = (x.toString + y.toString + z.toString + f.toString).hashCode

  def isNeighbour(other: HyperCoordinate): Boolean = {
    !this.equals(other) && !(
      Math.abs(this.x - other.x) > 2 ||
        Math.abs(this.y - other.y) > 2 ||
        Math.abs(this.z - other.z) > 2 ||
        Math.abs(this.f - other.f) > 2
      )
  }
}
