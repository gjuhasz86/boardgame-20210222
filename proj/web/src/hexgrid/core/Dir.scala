package hexgrid.core

sealed abstract class Dir(val idx: Int, val xOffs: Double, val yOffs: Double, val rOffs: Int, val cOffs: Int) {
  def rotate(n: Int): Dir = {
    val count = Dirs.all.size
    Dirs.byIdx(((n + idx) % count + count) % count)
  }
  def opposite: Dir = Dirs.byIdx(idx + 3)

  def rotateRight: Dir = rotate(1)
  def rotateLeft: Dir = rotate(-1)
}

object Dirs {
  case object UR extends Dir(0, 0.5, -0.5, -1, 0)
  case object RR extends Dir(1, 1.0, 0.0, 0, 1)
  case object DR extends Dir(2, 0.5, 0.5, 1, 1)
  case object DL extends Dir(3, -0.5, 0.5, 1, 0)
  case object LL extends Dir(4, -1.0, 0.0, 0, -1)
  case object UL extends Dir(5, -0.5, -0.5, -1, -1)

  val all: Seq[Dir] = List(UR, RR, DR, DL, LL, UL).sortBy(_.idx)
  def byIdx(idx: Int): Dir = all.find(_.idx == idx % all.size).get
}
