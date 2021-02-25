package hexgrid.drawables
import hexgrid.core.Monster
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos


object MonsterDrawable {

  implicit def monsterDrawable(implicit dc: DrawContext): Drawable[Monster] =
    (self: Monster, pos: ScreenPos) => {
      dc.ctx.lineWidth = 1
      dc.ctx.strokeStyle = "black"
      dc.ctx.fillStyle = "white"
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
}