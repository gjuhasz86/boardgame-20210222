package hexgrid.drawables
import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.core.Tiles.GameTile
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos

object TileDrawable {

  implicit def tileDrawable(implicit dc: DrawContext): Drawable[Tile] =
    (self: Tile, pos: ScreenPos) => {

      val innerTile: Tile = self match {
        case Tiles.VirtualTile(inner) =>
          dc.ctx.globalAlpha = 0.5
          inner
        case gt: GameTile =>
          dc.ctx.globalAlpha = 1.0
          gt
        case blank@Tiles.Blank =>
          dc.ctx.globalAlpha = 1.0
          blank
      }

      innerTile match {
        case path@Tiles.Path(_, _) =>
          dc.ctx.beginPath()
          dc.ctx.arc(pos.x, pos.y, dc.tileSize, 0, Math.PI * 2)
          dc.ctx.lineWidth = 1
          dc.ctx.strokeStyle = "black"
          dc.ctx.fillStyle = "white"
          dc.ctx.fill()
          dc.ctx.stroke()

          dc.ctx.strokeStyle = "red"
          dc.ctx.lineWidth = 5
          path.rotatedDirs.foreach { dir =>
            dc.ctx.beginPath()
            dc.ctx.moveTo(pos.x, pos.y)
            dc.ctx.lineTo(pos.x + dc.tileSize * dir.xOffs, pos.y + dc.screenTranslator.rowDistance * dir.yOffs)
            dc.ctx.stroke()
          }
        case Tiles.Blank =>
          dc.ctx.beginPath()
          dc.ctx.arc(pos.x, pos.y, dc.tileSize, 0, Math.PI * 2)
          dc.ctx.lineWidth = 1
          dc.ctx.strokeStyle = "black"
          dc.ctx.fillStyle = "white"
          dc.ctx.fill()
          dc.ctx.stroke()

        case _ =>
      }
    }
}
