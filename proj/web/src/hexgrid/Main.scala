package hexgrid
import hexgrid.drawables.GameStateDrawable._
import hexgrid.drawables.GameManagerDrawable._
import hexgrid.drawables.TileDrawable._
import hexgrid.drawables.TileMapDrawable._
import hexgrid.drawables.MonsterDrawable._
import hexgrid.gui.DefaultScreenTranslator
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    import Drawable._

    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    var mousePos = ScreenPos(0, 0)

    implicit val dc: DrawContext = new DrawContext {
      override val ctx: CanvasRenderingContext2D = renderCtx
      override val tileSize: Int = 50
      override val screenTranslator: ScreenTranslator = DefaultScreenTranslator(canvas.width, canvas.height, tileSize)
      override def cursorPos: ScreenPos = mousePos
      override def tileStackPos: ScreenPos = ScreenPos(tileSize + 20, tileSize + 20)
      override def hintPos: ScreenPos = ScreenPos(tileSize * 2 + 40, 20)
    }

    val gameManager = GameManager(dc)

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      gameManager.drawTo(ScreenPos(0, 0))
    }

    canvas.onmousemove = (e: dom.MouseEvent) => {
      mousePos = ScreenPos(e.pageX, e.pageY)
      dom.window.requestAnimationFrame(updateScreen)
    }

    canvas.onmouseup = (_: dom.MouseEvent) => {
      gameManager.tryPerform(GuiAction.Click)
      dom.window.requestAnimationFrame(updateScreen)
    }

    dom.document.onkeyup = (e: dom.KeyboardEvent) => {
      gameManager.tryPerform(GuiAction.Key(e.keyCode))
      dom.window.requestAnimationFrame(updateScreen)
    }
  }

}
