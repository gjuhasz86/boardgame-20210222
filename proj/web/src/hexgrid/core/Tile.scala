package hexgrid.core

sealed trait Tile {
  def rotatedDirs: Set[Dir]

  def resetRotation: Tile
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

  sealed trait GameTile extends Tile {
    override def rotate(n: Int): GameTile
    override def rotateRight: GameTile = rotate(1)
    override def rotateLeft: GameTile = rotate(-1)
  }

  case class Path private(dirs: Set[Dir], rot: Int)(name: String) extends GameTile {
    override def resetRotation: Tile = copy(rot = 0)(name)
    override def rotate(n: Int): Path = copy(rot = rot + n)(name)
    override def rotatedDirs: Set[Dir] = dirs.map(_.rotate(rot))
    override def toString: String = s"[$name]"
  }

  import Dirs._

  val Blank = Path(Set(), 0)("O")
  val E = Path(Set(UR, RR, DR), 0)("E")
  val Lambda = Path(Set(UL, DL, DR), 0)("L")
  val Y = Path(Set(UL, UR, DL), 0)("Y")
  val Center = Path(Set(UR, DR, LL), 0)("C")

  val I = Path(Set(LL, RR), 0)("I")
  val Star = Path(Set(UR, RR, DR, DL, LL, UL), 0)("*")
  val regularTiles = List(E, Lambda, Y, Center)

}