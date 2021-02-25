package hexgrid.drawables

import hexgrid.GameManager
import hexgrid.core.GameState
import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.Drawable._
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._

object GameManagerDrawable {
  implicit def gameManagerDrawable(implicit gsd: Drawable[GameState], td: Drawable[Tile], dc: DrawContext): Drawable[GameManager] = new Drawable[GameManager] {

    private implicit val st: ScreenTranslator = dc.screenTranslator

    override def draw(self: GameManager, pos: ScreenPos): Unit = {
      drawGameState(self, pos)
      if (self.isPlacingTile) {
        drawTileOverlay(self, pos)
      }
      drawTileStack(self, pos)
      drawCursor()
    }

    private def drawGameState(self: GameManager, pos: ScreenPos): Unit = {
      self.state.drawTo(pos)
    }

    private def drawTileStack(self: GameManager, pos: ScreenPos): Unit = {
      (1 to 5).reverse.foreach { i =>
        Tiles.Blank.drawTo(dc.tileStackPos + ScreenPos(0, 3 * i))
      }

      if (self.isPlacingTile) {
        self.state.nextTile.getOrElse(Tiles.Blank).drawTo(dc.tileStackPos)
      } else {
        Tiles.Blank.drawTo(dc.tileStackPos)
      }
    }

    private def drawTileOverlay(self: GameManager, pos: ScreenPos): Unit = {
      val overlayTile = self.state.nextTile.map(Tiles.VirtualTile).getOrElse(Tiles.Blank)
      td.draw(overlayTile, dc.cursorPos.toTile.toScreen)
    }

    private def drawCursor(): Unit = {
      dc.ctx.beginPath()
      dc.ctx.arc(dc.cursorPos.x, dc.cursorPos.y, 10, 0, Math.PI * 2)
      dc.ctx.stroke()
    }
  }
}
