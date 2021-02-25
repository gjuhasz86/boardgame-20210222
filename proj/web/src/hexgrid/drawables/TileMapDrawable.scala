package hexgrid.drawables
import hexgrid.core.TileMap
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._


object TileMapDrawable {
  implicit def tileMapDrawable[T](implicit td: Drawable[T], dc: DrawContext): Drawable[TileMap[T]] =
    new Drawable[TileMap[T]] {

      private implicit val st: ScreenTranslator = dc.screenTranslator

      override def draw(self: TileMap[T], offset: ScreenPos): Unit = {
        self.tiles.foreach { case (pos, tile) =>
          td.draw(tile, pos.toScreen + offset)
        }
      }

    }
}
