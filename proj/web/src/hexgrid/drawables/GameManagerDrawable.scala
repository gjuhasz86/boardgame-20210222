package hexgrid.drawables

import hexgrid.GameManager
import hexgrid.GamePhase
import hexgrid.GamePhase.MoveMonster
import hexgrid.GamePhase.PlacingNextTile
import hexgrid.GuiAction.Click
import hexgrid.core.GameState
import hexgrid.core.Monster
import hexgrid.core.Tile
import hexgrid.core.Tiles
import hexgrid.gui.CanDecorate
import hexgrid.gui.CanDecorate._
import hexgrid.gui.Decorators.Highlighted
import hexgrid.gui.Decorators.Invalid
import hexgrid.gui.Decorators.Overlay
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.Drawable._
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._

object GameManagerDrawable {
  implicit def gameManagerDrawable[
    DM <: Drawable[Monster],
    DT <: Drawable[Tile],
  ](
    implicit gsd: Drawable[GameState],
    td: DT,
    md: DM,
    mdd: CanDecorate[DM],
    tdd: CanDecorate[DT],
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
        //        drawCursor(self)
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
          self.state.nextTile
            .filter(_ => self.isPlacingTile)
            .getOrElse(Tiles.Blank)

        if (mouseOver) {
          topTile.make(Highlighted).drawTo(dc.tileStackPos)
        } else {
          topTile.drawTo(dc.tileStackPos)
        }
      }

      private def drawTileOverlay(self: GameManager, pos: ScreenPos): Unit = {
        lazy val tile = self.state.nextTile.getOrElse(Tiles.Blank)
        val valid = self.isValid(self.toGameAction(Click))
        self.phase match {
          case PlacingNextTile(Some(pos)) =>
            tile.make(Overlay).drawTo(pos.toScreen)
          case PlacingNextTile(None) if valid =>
            tile.make(Overlay).drawTo(dc.cursorPos.toTile.toScreen)
          case PlacingNextTile(None) if !valid =>
            tile.make(Overlay, Invalid).drawTo(dc.cursorPos.toTile.toScreen)
          case _ =>
        }
      }

      private def drawMonsterOverlay(self: GameManager, offset: ScreenPos): Unit = {
        self.phase match {
          case MoveMonster(Some(pos), _) =>
            self.state.monsters.tiles(pos).make(Highlighted).drawTo(pos.toScreen)
          case _ =>
        }

        self.phase match {
          case MoveMonster(Some(from), Some(to)) =>
            self.state.monsters.tiles(from).make(Overlay).drawTo(to.toScreen)
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
          case GameOver(p) => s"[GAMEOVER] | Player ${p.id} wins"
        }

        val textColor = self.phase match {
          case GameOver(_) => "white"
          case _ => "black"
        }

        dc.ctx.globalAlpha = 1.0
        dc.ctx.lineWidth = 1
        dc.ctx.strokeStyle = "black"
        dc.ctx.fillStyle = self.state.nextPlayer.color
        dc.ctx.beginPath()
        dc.ctx.rect(dc.hintPos.x, dc.hintPos.y, 450, 30)
        dc.ctx.fill()
        dc.ctx.stroke()


        dc.ctx.fillStyle = textColor
        dc.ctx.font = "14px Georgia"
        dc.ctx.textBaseline = "middle"
        dc.ctx.textAlign = "left"
        dc.ctx.fillText(phaseText, dc.hintPos.x + 5, dc.hintPos.y + 15)
      }

      private def drawCursor(self: GameManager): Unit = {
        if (!self.isPlacingTile) {
          Tiles.Blank.make(Overlay, Highlighted).drawTo(dc.cursorPos.toTile.toScreen)
        }
        dc.ctx.fillStyle = "black"
        dc.ctx.textBaseline = "middle"
        dc.ctx.font = "14px Georgia"

        val tp = dc.cursorPos.toTile
        val text = s"(${tp.r},${tp.c})"
        val width = dc.ctx.measureText(text).width

        dc.ctx.clearRect(dc.cursorPos.x + 10, dc.cursorPos.y + 20, width + 10, 20)
        dc.ctx.fillText(text, dc.cursorPos.x + 15, dc.cursorPos.y + 30)
      }
    }
}
