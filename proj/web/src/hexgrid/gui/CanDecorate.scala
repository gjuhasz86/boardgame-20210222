package hexgrid.gui
import hexgrid.gui.Drawable.DrawOnDrawable

trait CanDecorate[A] {
  def highlight(a: A): A
  def overlay(a: A): A
}
object CanDecorate {
  implicit class CanDecorateOps[A,+DA<:Drawable[A]](self: A)(implicit dr: DA, d: CanDecorate[DA]) {
    def highlight: DrawOnDrawable[A] = new DrawOnDrawable(self)(d.highlight(dr))
    def overlay: DrawOnDrawable[A] = new DrawOnDrawable(self)(d.overlay(dr))
  }
}