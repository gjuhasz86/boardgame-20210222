package hexgrid.gui

trait Drawable[-T] {
  def draw(self: T, pos: ScreenPos): Unit
}
object Drawable {
  implicit class DrawOnDrawable[T](self: T)(implicit d: Drawable[T]) {
    def drawTo(pos: ScreenPos): Unit = d.draw(self, pos)
  }
}