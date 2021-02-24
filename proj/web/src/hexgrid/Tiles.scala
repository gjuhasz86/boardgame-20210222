package hexgrid

object Tiles {
  case object Blank extends Tile {
    override def rotate(n: Int): Tile = this
  }
  case class VirtualTile(inner: GameTile) extends Tile {
    override def rotate(n: Int): Tile = copy(inner.rotate(n))
  }

  sealed trait GameTile extends Tile {
    override def rotate(n: Int): GameTile
    override def rotateRight: GameTile = rotate(1)
    override def rotateLeft: GameTile = rotate(-1)
  }

  case class Path private(dirs: Set[Dir], rot: Int) extends GameTile {
    override def rotate(n: Int): Path = copy(rot = rot + n)
    def rotatedDirs: Set[Dir] = dirs.map(_.rotate(rot))
  }

  import Dirs._

  val E = Path(Set(UR, RR, DR), 0)
  val Lambda = Path(Set(UL, DL, DR), 0)
  val Y = Path(Set(UL, UR, DR), 0)
  val Center = Path(Set(UR, DR, LL), 0)

  val gameTiles = List(E, Lambda, Y, Center)

}