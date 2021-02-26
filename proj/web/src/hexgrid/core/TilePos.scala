package hexgrid.core

case class TilePos(r: Int, c: Int) {

  def +(that: TilePos): TilePos =
    TilePos(r + that.r, c + that.c)

  def *(a: Int): TilePos =
    TilePos(r * a, c * a)

  def neighbor(dir: Dir): TilePos =
    TilePos(r + dir.rOffs, c + dir.cOffs)

  def distanceTo(that: TilePos): Int =
    (Math.abs(this.c - that.c)
      + Math.abs(this.c + this.r - that.c - that.r)
      + Math.abs(this.r - that.r)) / 2

  def rings(maxDistance: Int): Set[TilePos] =
    (0 to maxDistance).flatMap(ring).toSet

  def ring(distance: Int): Set[TilePos] = {
    import Dirs._

    val start = this + (TilePos(0, 0).neighbor(LL) * distance)
    val steps = Dirs.all.flatMap(dir => List.fill(distance)(dir))
    steps.scanLeft(start)((p, d) => p.neighbor(d)).toSet
  }
}