package hexgrid.core

case class TilePos(r: Int, c: Int) {
  def neighbor(dir: Dir): TilePos =
    TilePos(r + dir.rOffs, c + dir.cOffs)

  def distanceTo(that: TilePos): Int =
    (Math.abs(this.c - that.c)
      + Math.abs(this.c + this.r - that.c - that.r)
      + Math.abs(this.r - that.r)) / 2
}