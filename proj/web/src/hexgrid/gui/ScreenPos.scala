package hexgrid.gui

case class ScreenPos(x: Int, y: Int) {

  def +(pos: ScreenPos): ScreenPos =
    ScreenPos(x + pos.x, y + pos.y)

  def distanceTo(pos: ScreenPos): Int = {
    val dx = Math.abs(x - pos.x)
    val dy = Math.abs(y - pos.y)
    Math.sqrt(dx * dx + dy * dy).toInt
  }
}
object ScreenPos {
  def apply(x: Double, y: Double): ScreenPos = new ScreenPos(x.toInt, y.toInt)
}