package hexgrid
import hexgrid.Tiles.GameTile

import scala.util.Random

case class GameState(tileMap: TileMap, tileStack: List[GameTile]) {
  def placeNext(pos: TilePos) = tileStack match {
    case tile :: rest => place(pos, tile).copy(tileStack = rest)
    case _ => this
  }

  def place(pos: TilePos, tile: GameTile): GameState = copy(tileMap = tileMap.place(pos, tile))
  def nextTile: Option[GameTile] = tileStack.headOption
  def changeNextTile(f: GameTile => GameTile): GameState = tileStack match {
    case tile :: rest => copy(tileStack = f(tile) :: rest)
    case _ => this
  }
}

object GameState {
  def random(): GameState = {
    val tileStack = Random.shuffle(List.fill(25)(Tiles.gameTiles).flatten)
    new GameState(TileMap.empty, tileStack)
  }
}