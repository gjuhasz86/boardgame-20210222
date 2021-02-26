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
    (monsters.tiles.get(from), monsters.tiles.get(to)) match {
      case (Some(m1), Some(m2)) =>
        val m = List(m1, m2).maxBy(_.level)
        val newMonsters = monsters.remove(from).place(to, m)
        val alivePlayers = newMonsters.tiles.values.map(_.owner).toSet
        copy(monsters = newMonsters, playerTurns = playerTurns.filter(alivePlayers.contains))
      case (_, _) =>
        copy(monsters = monsters.move(from, to))
    }

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

  def activeMonsters: Set[TilePos] =
    monstersOfPlayer(nextPlayer)

  def monstersOfPlayer(p: Player): Set[TilePos] =
    monsters.tiles
      .filter { case (pos, m) => m.owner == p }
      .keySet


  def tileAt(pos: TilePos): Option[GameTile] =
    tileMap.tiles.get(pos)

  def validTiles(pos: TilePos): Seq[Tile] =
    Tiles.regularTiles
      .flatMap(tile => (0 to 5).map(n => tile.rotate(n)))
      .filter(validPlacement(pos, _))

  def validPlacement(pos: TilePos, tile: Tile): Boolean =
    tileMap.tiles.get(pos) match {
      case Some(_) => false
      case None =>
        val neighbors = Dirs.all.flatMap(dir => tileMap.tiles.get(pos.neighbor(dir)).map(dir -> _))
        neighbors.nonEmpty &&
          neighbors.forall { case (d, t) => tile.canPlaceNextTo(t, d) } &&
          neighbors.exists { case (d, t) => tile.isJoined(t, d) }
    }

  def neighbors(pos: TilePos): Seq[(Dir, GameTile)] =
    Dirs.all.flatMap(dir => tileMap.tiles.get(pos.neighbor(dir)).map(dir -> _))

  def canMove(from: TilePos, to: TilePos): Boolean =
    Dirs.all.filter(d => from.neighbor(d) == to)
      .exists(d => canMove(from, d))

  def canMove(pos: TilePos, dir: Dir): Boolean =
    (tileAt(pos), tileAt(pos.neighbor(dir))) match {
      case (Some(from), Some(to)) => from.isJoined(to, dir)
      case _ => false
    }
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