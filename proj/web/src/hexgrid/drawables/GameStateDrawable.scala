package hexgrid.drawables
import hexgrid.core.GameState
import hexgrid.core.Monster
import hexgrid.core.Tile
import hexgrid.core.TileMap
import hexgrid.gui.Drawable
import hexgrid.gui.Drawable._
import hexgrid.gui.ScreenPos

object GameStateDrawable {

  implicit def gameStateDrawable(
    implicit tmd: Drawable[TileMap[Tile]],
    md: Drawable[TileMap[Monster]]): Drawable[GameState] =
    new Drawable[GameState] {

      override def draw(self: GameState, pos: ScreenPos): Unit = {
        drawTileMap(self, pos)
        drawMonsters(self, pos)
      }

      private def drawTileMap(self: GameState, pos: ScreenPos): Unit = {
        self.tileMap.drawTo(pos)
      }

      private def drawMonsters(self: GameState, pos: ScreenPos): Unit = {
        self.monsters.drawTo(pos)
      }

    }
}
