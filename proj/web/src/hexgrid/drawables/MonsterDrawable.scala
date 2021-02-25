package hexgrid.drawables
import hexgrid.core.Monster
import hexgrid.gui.CanDecorate
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos


case class MonsterDrawable(highlight: Boolean, overlay: Boolean)(implicit dc: DrawContext) extends Drawable[Monster] {
  override def draw(self: Monster, pos: ScreenPos): Unit = {
    dc.ctx.lineWidth = 1
    dc.ctx.strokeStyle = "black"
    dc.ctx.globalAlpha = if (overlay) 0.5 else 1.0
    dc.ctx.fillStyle = if (highlight) "yellow" else "white"
    dc.ctx.textBaseline = "middle"
    dc.ctx.textAlign = "center"
    dc.ctx.font = "14px Georgia"
    dc.ctx.beginPath()
    dc.ctx.arc(pos.x, pos.y, dc.tileSize / 3, 0, Math.PI * 2)
    dc.ctx.fill()
    dc.ctx.stroke()
    dc.ctx.fillStyle = "black"
    dc.ctx.fillText(self.owner.id.toString, pos.x, pos.y)
  }

  def withHighlight: MonsterDrawable = this.copy(highlight = true, overlay = overlay)
  def withOverlay: MonsterDrawable = this.copy(highlight, overlay = true)
}

object MonsterDrawable {

  implicit def monsterDrawable(implicit dc: DrawContext): MonsterDrawable =
    new MonsterDrawable(false, false)(dc)

  implicit def canHighlightMonster(implicit dc: DrawContext): CanDecorate[MonsterDrawable] = new CanDecorate[MonsterDrawable] {
    override def highlight(a: MonsterDrawable): MonsterDrawable = a.withHighlight
    override def overlay(a: MonsterDrawable): MonsterDrawable = a.withOverlay
  }

}