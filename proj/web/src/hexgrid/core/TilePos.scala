package hexgrid.core

case class TilePos(r: Int, c: Int) {
  def neighbor(dir: Dir): TilePos =
    TilePos(r + dir.rOffs, c + dir.cOffs)
}