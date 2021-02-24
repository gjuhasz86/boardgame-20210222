package hexgrid.gui
import org.scalajs.dom

trait DrawContext {
  def ctx: dom.CanvasRenderingContext2D
  def tileSize: Int
  def screenTranslator: ScreenTranslator
  def cursorPos: ScreenPos
}