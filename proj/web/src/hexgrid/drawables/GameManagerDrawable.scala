package hexgrid.drawables

import hexgrid.GameManager
import hexgrid.GamePhase
import hexgrid.GamePhase.MoveMonster
import hexgrid.GamePhase.PlacingNextTile
import hexgrid.core.GameState
import hexgrid.core.Monster
import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.gui.CanDecorate
import hexgrid.gui.CanDecorate._
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.Drawable._
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._

object GameManagerDrawable {
  implicit def gameManagerDrawable[DM <: Drawable[Monster]](
    implicit gsd: Drawable[GameState],
    td: Drawable[Tile],
    md: DM,
    mdd: CanDecorate[DM],
    dc: DrawContext
  ): Drawable[GameManager] =
    new Drawable[GameManager] {

      private implicit val st: ScreenTranslator = dc.screenTranslator

      override def draw(self: GameManager, pos: ScreenPos): Unit = {
        drawGameState(self, pos)
        drawTileOverlay(self, pos)
        drawMonsterOverlay(self, pos)
        drawHints(self)
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
            overlayTile.drawTo(pos.toScreen)
          case PlacingNextTile(None) =>
            overlayTile.drawTo(dc.cursorPos.toTile.toScreen)
          case _ =>
        }
      }

      private def drawMonsterOverlay(self: GameManager, offset: ScreenPos): Unit = {
        self.phase match {
          case MoveMonster(Some(pos), _) =>
            self.state.monsters.tiles(pos).highlight.drawTo(pos.toScreen)
          case _ =>
        }

        self.phase match {
          case MoveMonster(Some(from), Some(to)) =>
            self.state.monsters.tiles(from).overlay.drawTo(to.toScreen)
          case _ =>
        }
      }

      private def drawHints(self: GameManager): Unit = {
        import GamePhase._

        val phaseText = self.phase match {
          case Idle => "[IDLE] | CLICK on a monster to move | CLICK on the tile stack to draw a tile"
          case MoveMonster(None, _) => "[MOVE] | CLICK on a monster to move | BACKSPACE to cancel"
          case MoveMonster(Some(_), None) => "[MOVE] | CLICK on a tile to select a target | BACKSPACE to cancel"
          case MoveMonster(Some(_), Some(_)) => "[MOVE] | ENTER to confirm monster placement | BACKSPACE to cancel"
          case PlacingNextTile(None) => "[TILE] | Click on an empty space to place the tile"
          case PlacingNextTile(Some(_)) => "[TILE] | ENTER to confirm tile placement | BACKSPACE to cancel"
        }

        dc.ctx.globalAlpha = 1.0
        dc.ctx.lineWidth = 1
        dc.ctx.strokeStyle = "black"
        dc.ctx.fillStyle = "white"
        dc.ctx.beginPath()
        dc.ctx.rect(dc.hintPos.x, dc.hintPos.y, 450, 30)
        dc.ctx.fill()
        dc.ctx.stroke()


        dc.ctx.fillStyle = "black"
        dc.ctx.font = "14px Georgia"
        dc.ctx.textBaseline = "middle"
        dc.ctx.textAlign = "left"
        dc.ctx.fillText(phaseText, dc.hintPos.x + 5, dc.hintPos.y + 15)
      }

      private def drawCursor(): Unit = {
        dc.ctx.beginPath()
        dc.ctx.arc(dc.cursorPos.x, dc.cursorPos.y, 10, 0, Math.PI * 2)
        dc.ctx.stroke()
      }
    }
}
