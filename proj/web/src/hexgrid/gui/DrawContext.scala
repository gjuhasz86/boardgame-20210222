package hexgrid.gui
import org.scalajs.dom

trait DrawContext {
  def ctx: dom.CanvasRenderingContext2D
  def tileSize: Int
  def cursorPos: ScreenPos
  def tileStackPos: ScreenPos
  def hintPos: ScreenPos
}