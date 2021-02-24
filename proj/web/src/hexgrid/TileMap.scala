package hexgrid
import hexgrid.Tiles.GameTile

case class TileMap(tiles: Map[TilePos, GameTile]) {
  def place(r: Int, c: Int, tile: GameTile): TileMap =
    place(TilePos(r, c), tile)

  def place(pos: TilePos, tile: GameTile): TileMap =
    copy(tiles = tiles + (pos -> tile))
}

object TileMap {
  def empty = TileMap(Map.empty)
}