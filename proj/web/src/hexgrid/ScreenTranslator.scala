package hexgrid

trait ScreenTranslator {
  def rowDistance: Double
  def tileToScreen(pos: TilePos): ScreenPos
  def screenToTile(pos: ScreenPos): TilePos
}
object ScreenTranslator {
  implicit class ToScreenOnTilePos(self: TilePos)(implicit st: ScreenTranslator) {
    def toScreen: ScreenPos = st.tileToScreen(self)
  }

  implicit class ToTileOnScreenPos(self: ScreenPos)(implicit st: ScreenTranslator) {
    def toTile: TilePos = st.screenToTile(self)
  }
}

case class DefaultScreenTranslator(screenWidth: Int, screenHeight: Int, size: Int) extends ScreenTranslator {
  val rowDistance = Math.sqrt((size * size * 4) - size * size)
  val origin = ScreenPos(screenWidth / 2, screenHeight / 2)

  override def tileToScreen(pos: TilePos): ScreenPos = {
    val offset = if (pos.r % 2 == 0) 0 else size
    ScreenPos(origin.x + offset + size * pos.c * 2, (origin.y + rowDistance * pos.r).toInt)
  }

  override def screenToTile(pos: ScreenPos): TilePos = {
    val r = Math.round((pos.y - origin.y) / rowDistance).toInt
    val offset = if (r % 2 == 0) 0 else size
    val c = Math.round((pos.x - origin.x - offset) / (size.toDouble * 2)).toInt
    TilePos(r, c)
  }
}
