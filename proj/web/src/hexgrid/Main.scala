package hexgrid
import hexgrid.core.GameState
import hexgrid.drawables.GameStateDrawable._
import hexgrid.drawables.TileDrawables._
import hexgrid.drawables.TileMapDrawable._
import hexgrid.gui.DefaultScreenTranslator
import hexgrid.gui.DrawContext
import hexgrid.gui.Drawable
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    import Drawable._
    import ScreenTranslator._

    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    var mousePos = ScreenPos(0, 0)

    implicit val dc = new DrawContext {
      override val ctx: CanvasRenderingContext2D = renderCtx
      override val tileSize: Int = 50
      override val screenTranslator: ScreenTranslator = DefaultScreenTranslator(canvas.width, canvas.height, tileSize)
      override def cursorPos: ScreenPos = mousePos
    }

    var gameState = GameState.random()
    def updateGameState(f: GameState => GameState): Unit = {
      gameState = f(gameState)
    }

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      gameState.drawTo(ScreenPos(0, 0))
    }

    canvas.onmousemove = (e: dom.MouseEvent) => {
      mousePos = ScreenPos(e.pageX, e.pageY)
      dom.window.requestAnimationFrame(updateScreen)
    }

    canvas.onmouseup = (_: dom.MouseEvent) => {
      implicit val st = dc.screenTranslator
      updateGameState(_.placeNext(dc.cursorPos.toTile))
    }

    dom.document.onkeyup = (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case KeyCode.E => updateGameState(_.changeNextTile(x => x.rotateRight))
        case KeyCode.Q => updateGameState(_.changeNextTile(x => x.rotateLeft))
      }
      dom.window.requestAnimationFrame(updateScreen)
    }
  }

}
