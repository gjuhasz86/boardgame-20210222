package hexgrid.drawables
import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.core.Tiles.GameTile
import hexgrid.gui.DrawContextHolder
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import org.scalajs.dom

object TileDrawables {
  type Dch = DrawContextHolder[dom.CanvasRenderingContext2D]

  implicit def tileDrawable(implicit dch: Dch): Drawable[Tile] =
    (self: Tile, pos: ScreenPos) => {

      val innerTile: Tile = self match {
        case Tiles.VirtualTile(inner) =>
          dch.ctx.globalAlpha = 0.5
          inner
        case gt: GameTile =>
          dch.ctx.globalAlpha = 1.0
          gt
        case blank@Tiles.Blank =>
          dch.ctx.globalAlpha = 1.0
          blank
      }

      innerTile match {
        case path@Tiles.Path(_, _) =>
          dch.ctx.beginPath()
          dch.ctx.arc(pos.x, pos.y, dch.tileSize, 0, Math.PI * 2)
          dch.ctx.lineWidth = 1
          dch.ctx.strokeStyle = "black"
          dch.ctx.fillStyle = "white"
          dch.ctx.fill()
          dch.ctx.stroke()

          dch.ctx.strokeStyle = "red"
          dch.ctx.lineWidth = 5
          path.rotatedDirs.foreach { dir =>
            dch.ctx.beginPath()
            dch.ctx.moveTo(pos.x, pos.y)
            dch.ctx.lineTo(pos.x + dch.tileSize * dir.xOffs, pos.y + dch.screenTranslator.rowDistance * dir.yOffs)
            dch.ctx.stroke()
          }
        case _ =>
      }
    }
}
