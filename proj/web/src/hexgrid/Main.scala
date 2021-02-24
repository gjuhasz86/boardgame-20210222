package hexgrid
import hexgrid.core.GameState
import hexgrid.core.Tile
import hexgrid.core.TileMap
import hexgrid.core.Tiles
import hexgrid.core.Tiles.GameTile
import hexgrid.gui.DefaultScreenTranslator
import hexgrid.gui.DrawContextHolder
import hexgrid.gui.Drawable
import hexgrid.gui.GameOverlay
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import hexgrid.drawables.GameOverlayDawable._
import hexgrid.drawables.GameStateDrawable._
import hexgrid.drawables.TileDrawables._
import hexgrid.drawables.TileMapDrawable._

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

    implicit val dch = new DrawContextHolder[dom.CanvasRenderingContext2D] {
      override def ctx: CanvasRenderingContext2D = renderCtx
      override def tileSize: Int = 50
      override def screenTranslator: ScreenTranslator = DefaultScreenTranslator(canvas.width, canvas.height, tileSize)
    }

    var overlay = GameOverlay(ScreenPos(0, 0), GameState.random())

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      overlay.gameState.drawTo(ScreenPos(0, 0))
      overlay.drawTo(ScreenPos(0, 0))
    }

    canvas.onmousemove = (e: dom.MouseEvent) => {
      overlay = overlay.setMouse(ScreenPos(e.pageX, e.pageY))
      dom.window.requestAnimationFrame(updateScreen)
    }

    canvas.onmouseup = (_: dom.MouseEvent) => {
      implicit val st = dch.screenTranslator
      overlay = overlay.changeState(_.placeNext(overlay.mousePos.toTile))
    }

    dom.document.onkeyup = (e: dom.KeyboardEvent) => {
      val newOverlay = e.keyCode match {
        case KeyCode.E => overlay.changeState(_.changeNextTile(x => x.rotateRight))
        case KeyCode.Q => overlay.changeState(_.changeNextTile(x => x.rotateLeft))
      }
      overlay = newOverlay
      dom.window.requestAnimationFrame(updateScreen)
    }
  }

}
