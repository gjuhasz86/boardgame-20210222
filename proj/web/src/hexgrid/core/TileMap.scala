package hexgrid.core

case class TileMap[+A](tiles: Map[TilePos, A]) {
  def place[B >: A](r: Int, c: Int, tile: B): TileMap[B] =
    place(TilePos(r, c), tile)

  def place[B >: A](pos: TilePos, tile: B): TileMap[B] =
    copy(tiles = tiles + (pos -> tile))

  def move(from: TilePos, to: TilePos): TileMap[A] = {
    val newMap = tiles.get(from).map(t => Map(to -> t)).getOrElse(Map.empty)
    copy(tiles = (tiles - from) ++ newMap)
  }

  def change[B >: A](pos: TilePos)(f: A => B): TileMap[B] =
    tiles.get(pos) match {
      case Some(a) => place(pos, f(a))
      case None => this
    }
}

object TileMap {
  def empty[T]: TileMap[T] = TileMap(Map.empty)
}