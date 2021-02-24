package hexgrid.drawables
import hexgrid.core.GameState
import hexgrid.core.Tile
import hexgrid.core.TileMap
import hexgrid.core.Tiles
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.Drawable._
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._

object GameStateDrawable {

  implicit def gameStateDrawable(implicit tmd: Drawable[TileMap], td: Drawable[Tile], dc: DrawContext): Drawable[GameState] =
    new Drawable[GameState] {
      private implicit val st: ScreenTranslator = dc.screenTranslator

      override def draw(self: GameState, pos: ScreenPos): Unit = {
        drawTileMap(self, pos)
        drawOverlay(self, pos)
        drawCursor(self, pos)
      }

      private def drawTileMap(self: GameState, pos: ScreenPos): Unit = {
        self.tileMap.drawTo(pos)
      }

      private def drawOverlay(self: GameState, pos: ScreenPos): Unit = {
        val overlayTile = self.nextTile.map(Tiles.VirtualTile).getOrElse(Tiles.Blank)
        td.draw(overlayTile, dc.cursorPos.toTile.toScreen)
      }

      private def drawCursor(self: GameState, pos: ScreenPos): Unit = {
        dc.ctx.beginPath()
        dc.ctx.arc(dc.cursorPos.x, dc.cursorPos.y, 10, 0, Math.PI * 2)
        dc.ctx.stroke()
      }

    }
}
