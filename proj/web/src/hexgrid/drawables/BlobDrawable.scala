package hexgrid.drawables
import hexgrid.core.Blob
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos

case class BlobDrawable()(implicit dc: DrawContext) extends Drawable[Blob.type] {
  override def draw(a: Blob.type, pos: ScreenPos): Unit = {
    dc.ctx.lineWidth = 1
    dc.ctx.strokeStyle = "black"
    dc.ctx.fillStyle = "#2196F3"
    dc.ctx.beginPath()
    dc.ctx.arc(pos.x + dc.tileSize / 2, pos.y + dc.tileSize / 2, dc.tileSize / 4, 0, Math.PI * 2)
    dc.ctx.fill()
    dc.ctx.stroke()
  }
}

object BlobDrawable {

  implicit def blobDrawable(implicit dc: DrawContext): BlobDrawable = new BlobDrawable()

}