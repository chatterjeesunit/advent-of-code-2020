package day20

case class Tile (tileId: String, top: String, bottom: String, left: String, right: String) {

  def rotateClockWise(i:Int, n:Int): Tile = {
    if(i == n) {
      return this
    }
    this.copy(top = this.left.reverse, right = this.top, bottom = this.right.reverse, left = this.bottom).rotateClockWise(i+1, n)
  }


  def flipVertical: Tile = this.copy(top = this.bottom, bottom = this.top, left = this.left.reverse, right=this.right.reverse)

  def flipHorizontal: Tile = this.copy(left = this.right, right = this.left, bottom = this.bottom.reverse, top = this.top.reverse)

  def getPossibleOrientationsForHorizontalMatch(): List[Tile] = {
    List(
      this,
      flipHorizontal, flipVertical,
      rotateClockWise(0,1), rotateClockWise(0,2), rotateClockWise(0,3),
      flipHorizontal.rotateClockWise(0,1), flipHorizontal.rotateClockWise(0,2),
      flipVertical.rotateClockWise(0,1), flipVertical.rotateClockWise(0,2))
  }

  def getPossibleOrientationsForVerticalMatch(): List[Tile] = {
    List(
      this,
      flipVertical,
      flipHorizontal,
      flipVertical.flipHorizontal
    )
  }

  def matchHorizontal(other: Tile) = {
    val possibleOrientations = other.getPossibleOrientationsForHorizontalMatch()
    var i = 0
    var matchedTile: Option[Tile] = None
    while(matchedTile.isEmpty && i < possibleOrientations.size) {
      val t = possibleOrientations(i)
      if(left.equals(t.right)) matchedTile = Some(joinHorizontal(t, this))
      else if(right.equals(t.left)) matchedTile = Some(joinHorizontal(this, t))
      else i = i + 1
    }
    matchedTile
  }

  def matchVertical(other: Tile) = {
    val possibleOrientations = other.getPossibleOrientationsForVerticalMatch()
    var i = 0
    var matchedTile: Option[Tile] = None
    while(matchedTile.isEmpty && i < possibleOrientations.size) {
      val t = possibleOrientations(i)
      if(bottom.equals(t.top)) matchedTile = Some(joinVertical(this, t))
      else if(top.equals(t.bottom)) matchedTile = Some(joinVertical(t, this))
      else i = i + 1
    }
    matchedTile
  }

  def joinHorizontal(tile1: Tile, tile2: Tile): Tile = {
    Tile(tile1.tileId+","+tile2.tileId, tile1.top+tile2.top, tile1.bottom+tile2.bottom, tile1.left, tile2.right)
  }
  def joinVertical(tile1: Tile, tile2: Tile): Tile = {
    Tile(tile1.tileId+";"+tile2.tileId, tile1.top, tile2.bottom, tile1.left+tile2.left, tile1.right+tile2.right)
  }

  def isTileLengthMaxed(n: Int) = tileId.split(",").length >= n

  def isTileHieghtMaxed(n: Int) = tileId.split(";").length >= n

  override def toString: String = this.tileId
}
