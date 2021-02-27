package hexgrid.drawables
import hexgrid.core.Monster
import hexgrid.gui.CanDecorate
import hexgrid.gui.Decorator
import hexgrid.gui.Decorators.Empty
import hexgrid.gui.Decorators.Highlighted
import hexgrid.gui.Decorators.Overlay
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos

case class MonsterDrawable(decorators: Set[Decorator])(implicit dc: DrawContext) extends Drawable[Monster] {
  private val alpha = if (is(Overlay)) 0.5 else 1.0
  private def is(d: Decorator) = decorators.contains(d)

  override def draw(self: Monster, pos: ScreenPos): Unit = {
    val bkgColor = if (decorators.contains(Highlighted)) "yellow" else self.owner.color
    dc.ctx.lineWidth = 1
    dc.ctx.strokeStyle = "black"
    dc.ctx.globalAlpha = alpha
    dc.ctx.fillStyle = bkgColor
    dc.ctx.textBaseline = "middle"
    dc.ctx.textAlign = "center"
    dc.ctx.font = "14px Georgia"
    dc.ctx.beginPath()
    dc.ctx.arc(pos.x, pos.y, dc.tileSize / 2, 0, Math.PI * 2)
    dc.ctx.fill()
    dc.ctx.stroke()
    dc.ctx.fillStyle = "black"
    if (!is(Empty)) {
      dc.ctx.fillText(s"${self.level}.${self.power}", pos.x, pos.y)
    }
  }

  def decorate(d: Decorator): MonsterDrawable = this.copy(decorators = decorators + d)
}

object MonsterDrawable {

  implicit def monsterDrawable(implicit dc: DrawContext): MonsterDrawable =
    new MonsterDrawable(Set.empty)(dc)

  implicit def canHighlightMonster(implicit dc: DrawContext): CanDecorate[MonsterDrawable] =
    (a: MonsterDrawable, d: Decorator) => a.decorate(d)

}