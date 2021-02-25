package hexgrid.gui

trait Drawable[-A] {
  def draw(a: A, pos: ScreenPos): Unit
}
object Drawable {
  implicit class DrawableOps[+T](val self: T)(implicit d: Drawable[T]) {
    def drawTo(pos: ScreenPos): Unit = d.draw(self, pos)
  }
}
