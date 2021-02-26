package hexgrid.drawables
import hexgrid.core.Blob
import hexgrid.core.GameState
import hexgrid.core.Monster
import hexgrid.core.Tile
import hexgrid.core.TileMap
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenTranslator._
import hexgrid.gui.Drawable._
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator

object GameStateDrawable {

  implicit def gameStateDrawable(
    implicit tmd: Drawable[TileMap[Tile]],
    md: Drawable[TileMap[Monster]],
    bd: Drawable[Blob.type],
    dc: DrawContext,
    st: ScreenTranslator
  ): Drawable[GameState] =
    new Drawable[GameState] {

      override def draw(self: GameState, pos: ScreenPos): Unit = {
        drawTileMap(self, pos)
        drawMonsters(self, pos)
        drawBlobs(self, pos)
      }

      private def drawTileMap(self: GameState, pos: ScreenPos): Unit = {
        self.tileMap.drawTo(pos)
      }

      private def drawMonsters(self: GameState, pos: ScreenPos): Unit = {
        self.monsters.drawTo(pos)
      }

      private def drawBlobs(self: GameState, pos: ScreenPos): Unit = {
        self.blobs.foreach(p => Blob.drawTo(p.toScreen + pos))
      }

    }
}
