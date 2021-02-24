package hexgrid.drawables
import hexgrid.core.Tile
import hexgrid.core.TileMap
import hexgrid.gui.DrawContextHolder
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import org.scalajs.dom


object TileMapDrawable {
  type Dch = DrawContextHolder[dom.CanvasRenderingContext2D]
  implicit def tileMapDrawable(implicit td: Drawable[Tile], dch: Dch): Drawable[TileMap] =
    (self: TileMap, pos: ScreenPos) => {
      implicit val st = dch.screenTranslator
      self.tiles.foreach { case (pos, tile) =>
        td.draw(tile, pos.toScreen)
      }
    }
}
