package hexgrid.core

import hexgrid.core.Tiles.GameTile

import scala.util.Random

case class Player(id: Int)
case class Monster(owner: Player)
case class GameState(
  tileMap: TileMap[GameTile],
  tileStack: List[GameTile],
  monsters: TileMap[Monster],
  playerTurns: List[Player]
) {

  def placeNext(pos: TilePos): GameState =
    tileStack match {
      case tile :: rest => place(pos, tile).copy(tileStack = rest)
      case _ => this
    }

  def place(pos: TilePos, tile: GameTile): GameState =
    copy(tileMap = tileMap.place(pos, tile))

  def moveMonster(from: TilePos, to: TilePos): GameState =
    copy(monsters = monsters.move(from, to))

  def nextTile: Option[GameTile] = tileStack.headOption
  def changeNextTile(f: GameTile => GameTile): GameState = tileStack match {
    case tile :: rest => copy(tileStack = f(tile) :: rest)
    case _ => this
  }
}

object GameState {
  def default(): GameState = {
    val p1 = Player(1)
    val p2 = Player(2)
    val tileStack = Random.shuffle(List.fill(25)(Tiles.regularTiles).flatten)
    val tileMap =
      TileMap.empty[GameTile]
        .place(0, 0, Tiles.I)
        .place(0, 1, Tiles.Star)
        .place(0, -1, Tiles.Star)
    val monsters =
      TileMap.empty[Monster]
        .place(0, 1, Monster(p1))
        .place(0, -1, Monster(p2))

    new GameState(tileMap, tileStack, monsters, List(p1, p2))
  }
}