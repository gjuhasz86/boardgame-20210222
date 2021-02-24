package hexgrid

sealed trait Tile {
  def rotate(n: Int): Tile
  def rotateRight: Tile = rotate(1)
  def rotateLeft: Tile = rotate(-1)
}