package hexgrid.gui
import hexgrid.gui.Drawable.DrawableOps

trait CanDecorate[A] {
  def decorate(a: A, d: Decorator): A
}
object CanDecorate {
  implicit class CanDecorateOps[A, +DA <: Drawable[A]](self: A)(implicit dr: DA, cd: CanDecorate[DA]) {
    def make(d: Decorator): DrawableOps[A] = new DrawableOps(self)(cd.decorate(dr, d))
    def make(ds: Decorator*): DrawableOps[A] = {
      val newDa = ds.foldLeft(dr)((drr, d) => cd.decorate(drr, d))
      new DrawableOps(self)(newDa)
    }
  }
}

sealed trait Decorator
object Decorators {
  case object Highlighted extends Decorator
  case object Overlay extends Decorator
  case object Invalid extends Decorator
}