package hexgrid.gui

trait DrawContextHolder[T] {
  def ctx: T
  def tileSize: Int
  def screenTranslator: ScreenTranslator
}