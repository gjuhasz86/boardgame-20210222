package hexgrid.drawables
import hexgrid.core.GameState
import hexgrid.core.TileMap
import hexgrid.gui.DrawContextHolder
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import org.scalajs.dom

object GameStateDrawable {
  import Drawable._

  type Dch = DrawContextHolder[dom.CanvasRenderingContext2D]
  implicit def gameStateDrawable(implicit tmd: Drawable[TileMap]): Drawable[GameState] =
    (self: GameState, pos: ScreenPos) => self.tileMap.drawTo(pos)
}
