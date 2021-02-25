package hexgrid.drawables

import hexgrid.GameManager
import hexgrid.GamePhase.PlacingNextTile
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
      drawTileOverlay(self, pos)
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

      val mouseOver = dc.cursorPos.distanceTo(dc.tileStackPos) < dc.tileSize

      val topTile =
        if (self.isPlacingTile)
          self.state.nextTile.getOrElse(Tiles.Blank)
        else if (mouseOver)
          Tiles.HighLightedTile(Tiles.Blank)
        else
          Tiles.Blank

      topTile.drawTo(dc.tileStackPos)
    }

    private def drawTileOverlay(self: GameManager, pos: ScreenPos): Unit = {
      lazy val overlayTile = self.state.nextTile.map(Tiles.VirtualTile).getOrElse(Tiles.Blank)
      self.phase match {
        case PlacingNextTile(Some(pos)) =>
          td.draw(overlayTile, pos.toScreen)
        case PlacingNextTile(None) =>
          td.draw(overlayTile, dc.cursorPos.toTile.toScreen)
        case _ =>
      }
    }

    private def drawCursor(): Unit = {
      dc.ctx.beginPath()
      dc.ctx.arc(dc.cursorPos.x, dc.cursorPos.y, 10, 0, Math.PI * 2)
      dc.ctx.stroke()
    }
  }
}
