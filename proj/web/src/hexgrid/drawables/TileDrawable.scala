package hexgrid.drawables
import java.lang.Math.PI

import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.gui.CanDecorate
import hexgrid.gui.Decorator
import hexgrid.gui.Decorators.Highlighted
import hexgrid.gui.Decorators.Invalid
import hexgrid.gui.Decorators.Overlay
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator

case class TileDrawable(decorators: Set[Decorator], dc: DrawContext, st: ScreenTranslator) extends Drawable[Tile] {

  def decorate(d: Decorator): TileDrawable = copy(decorators = decorators + d)

  private val alpha = if (decorators.contains(Overlay)) 0.5 else 1.0
  private val highlighted = decorators.contains(Highlighted)
  private val invalid = decorators.contains(Invalid)

  override def draw(tile: Tile, pos: ScreenPos): Unit = {

    dc.ctx.globalAlpha = alpha

    tile match {
      case path@Tiles.Path(_, _) =>
        val bkgColor = if (highlighted) "yellow" else if (invalid) "#F44336" else "#8BC34A"
        val edgeColor = "black"
        val pathColor = "#AD6800"

        // outer line
        dc.ctx.lineWidth = 1
        dc.ctx.strokeStyle = edgeColor
        dc.ctx.fillStyle = bkgColor

        dc.ctx.beginPath()
        dc.ctx.arc(pos.x, pos.y, dc.tileSize, 0, PI * 2)
        dc.ctx.fill()
        dc.ctx.stroke()

        // inner paths
        dc.ctx.lineWidth = 10
        dc.ctx.strokeStyle = pathColor
        dc.ctx.fillStyle = pathColor

        dc.ctx.beginPath()
        path.rotatedDirs.foreach { dir =>
          dc.ctx.moveTo(pos.x, pos.y)
          dc.ctx.lineTo(pos.x + dc.tileSize * dir.xOffs, pos.y + st.rowDistance * dir.yOffs)
        }
        dc.ctx.stroke()

        // middle path join
        dc.ctx.lineWidth = 1
        dc.ctx.strokeStyle = pathColor
        dc.ctx.fillStyle = pathColor
        dc.ctx.beginPath()
        dc.ctx.arc(pos.x, pos.y, 5, 0, PI * 2)
        dc.ctx.fill()

      case Tiles.Blank =>
        val color = if (highlighted) "yellow" else "white"
        dc.ctx.lineWidth = 1
        dc.ctx.strokeStyle = "black"
        dc.ctx.fillStyle = color

        dc.ctx.beginPath()
        dc.ctx.arc(pos.x, pos.y, dc.tileSize, 0, PI * 2)
        dc.ctx.fill()
        dc.ctx.stroke()

      case _ =>
    }
  }
}

object TileDrawable {

  implicit def tileDrawable(implicit dc: DrawContext, st: ScreenTranslator): TileDrawable =
    new TileDrawable(Set.empty, dc, st)

  implicit val canDecorateTileDrawable: CanDecorate[TileDrawable] =
    (a: TileDrawable, d: Decorator) => a.decorate(d)
}
