package hexgrid.drawables
import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.gui.DrawContextHolder
import hexgrid.gui.Drawable
import hexgrid.gui.GameOverlay
import hexgrid.gui.ScreenPos
import org.scalajs.dom

object GameOverlayDawable {
  import hexgrid.gui.ScreenTranslator._

  type Dch = DrawContextHolder[dom.CanvasRenderingContext2D]
  implicit def overlayDrawable(implicit td: Drawable[Tile], dch: Dch): Drawable[GameOverlay] =
    (self: GameOverlay, pos: ScreenPos) => {
      implicit val st = dch.screenTranslator

      val overlayTile = self.gameState.nextTile.map(Tiles.VirtualTile).getOrElse(Tiles.Blank)
      td.draw(overlayTile, self.mousePos.toTile.toScreen)

      dch.ctx.beginPath()
      dch.ctx.arc(self.mousePos.x, self.mousePos.y, 10, 0, Math.PI * 2)
      dch.ctx.stroke()
    }
}