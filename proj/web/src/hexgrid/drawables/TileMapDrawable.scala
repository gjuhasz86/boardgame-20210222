package hexgrid.drawables
import hexgrid.core.Tile
import hexgrid.core.TileMap
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator._


object TileMapDrawable {
  implicit def tileMapDrawable(implicit td: Drawable[Tile], dc: DrawContext): Drawable[TileMap] =
    (self: TileMap, pos: ScreenPos) => {
      implicit val st = dc.screenTranslator
      self.tiles.foreach { case (pos, tile) =>
        td.draw(tile, pos.toScreen)
      }
    }
}
