package hexgrid.core

sealed trait Tile {
  def rotatedDirs: Set[Dir]

  def rotate(n: Int): Tile
  def rotateRight: Tile = rotate(1)
  def rotateLeft: Tile = rotate(-1)

  def canPlaceNextTo(that: Tile, dir: Dir): Boolean =
    this.rotatedDirs.contains(dir) == that.rotatedDirs.contains(dir.opposite)

  def isJoined(that: Tile, dir: Dir): Boolean =
    this.rotatedDirs.contains(dir) && that.rotatedDirs.contains(dir.opposite)
}

//noinspection TypeAnnotation
object Tiles {
  case object Blank extends Tile {
    override def rotate(n: Int): Tile = this
    override def rotatedDirs: Set[Dir] = Set()
  }

  sealed trait GameTile extends Tile {
    override def rotate(n: Int): GameTile
    override def rotateRight: GameTile = rotate(1)
    override def rotateLeft: GameTile = rotate(-1)
  }

  case class Path private(dirs: Set[Dir], rot: Int) extends GameTile {
    override def rotate(n: Int): Path = copy(rot = rot + n)
    override def rotatedDirs: Set[Dir] = dirs.map(_.rotate(rot))
  }

  import Dirs._

  val E = Path(Set(UR, RR, DR), 0)
  val Lambda = Path(Set(UL, DL, DR), 0)
  val Y = Path(Set(UL, UR, DL), 0)
  val Center = Path(Set(UR, DR, LL), 0)

  val I = Path(Set(LL, RR), 0)
  val Star = Path(Set(UR, RR, DR, DL, LL, UL), 0)
  val regularTiles = List(E, Lambda, Y, Center)

}