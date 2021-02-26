package hexgrid.core

import hexgrid.core.Tiles.GameTile

import scala.util.Random

case object Blob
sealed trait Card
object Cards {
  case object PlaceBlob extends Card
  case object Skip extends Card

  def all = List(PlaceBlob, Skip)
}
case class GameState(
  tileMap: TileMap[GameTile],
  tileStack: List[GameTile],
  monsters: TileMap[Monster],
  playerTurns: List[Player],
  blobs: List[TilePos],
  cardStack: List[Card]
) {

  def drawCard: GameState =
    cardStack match {
      case _ :: rest =>
        copy(cardStack = rest)
      case Nil => this
    }

  def placeNext(pos: TilePos): GameState =
    tileStack match {
      case tile :: rest => place(pos, tile).copy(tileStack = rest)
      case _ => this
    }

  def place(pos: TilePos, tile: GameTile): GameState =
    copy(tileMap = tileMap.place(pos, tile))

  def moveMonster(from: TilePos, to: TilePos): GameState =
    copy(monsters = monsters.move(from, to))

  def changeMonster(pos: TilePos)(f: Monster => Monster): GameState =
    copy(monsters = monsters.change(pos)(f))

  def placeBlob(pos: TilePos): GameState = {
    copy(blobs = pos :: blobs)
  }

  def takeBlob(pos: TilePos): GameState = {
    copy(blobs = blobs diff List(pos))
  }

  def nextCard: Option[Card] = cardStack.headOption

  def nextTile: Option[GameTile] = tileStack.headOption
  def changeNextTile(f: GameTile => GameTile): GameState = tileStack match {
    case tile :: rest => copy(tileStack = f(tile) :: rest)
    case _ => this
  }

  def nextPlayer: Player = playerTurns.head
  def endTurn: GameState =
    copy(playerTurns = playerTurns.tail :+ playerTurns.head)

}

object GameState {
  def default(): GameState = {
    val p1 = Player(1, "#8E5EE3")
    val p2 = Player(2, "#FF9800")
    val tileStack = Random.shuffle(List.fill(25)(Tiles.regularTiles).flatten)
    val cardStack = Random.shuffle(List.fill(50)(Cards.all).flatten)
    val tileMap =
      TileMap.empty[GameTile]
        .place(0, 0, Tiles.I)
        .place(0, 1, Tiles.Star)
        .place(0, -1, Tiles.Star)
    val monsters =
      TileMap.empty[Monster]
        .place(0, 1, Monster(1, 0, p1))
        .place(0, -1, Monster(1, 0, p2))

    new GameState(tileMap, tileStack, monsters, List(p1, p2), Nil, cardStack)
  }
}