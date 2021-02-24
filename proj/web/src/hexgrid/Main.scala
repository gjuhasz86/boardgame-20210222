package hexgrid
import hexgrid.Tiles.GameTile
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    import Drawable._
    import Drawables._
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

case class TilePos(r: Int, c: Int)
case class ScreenPos(x: Int, y: Int)
object ScreenPos {
  def apply(x: Double, y: Double): ScreenPos = new ScreenPos(x.toInt, y.toInt)
}


trait Drawable[T] {
  def draw(self: T, pos: ScreenPos): Unit
}
object Drawable {
  implicit class DrawOnDrawable[T](self: T)(implicit d: Drawable[T]) {
    def drawTo(pos: ScreenPos): Unit = d.draw(self, pos)
  }
}
object Drawables {
  import Drawable._
  import ScreenTranslator._

  type Dch = DrawContextHolder[dom.CanvasRenderingContext2D]

  implicit def tileDrawable(implicit dch: Dch): Drawable[Tile] =
    (self: Tile, pos: ScreenPos) => {

      val innerTile: Tile = self match {
        case Tiles.VirtualTile(inner) =>
          dch.ctx.globalAlpha = 0.5
          inner
        case gt: GameTile =>
          dch.ctx.globalAlpha = 1.0
          gt
        case blank@Tiles.Blank =>
          dch.ctx.globalAlpha = 1.0
          blank
      }

      innerTile match {
        case path@Tiles.Path(_, _) =>
          dch.ctx.beginPath()
          dch.ctx.arc(pos.x, pos.y, dch.tileSize, 0, Math.PI * 2)
          dch.ctx.lineWidth = 1
          dch.ctx.strokeStyle = "black"
          dch.ctx.fillStyle = "white"
          dch.ctx.fill()
          dch.ctx.stroke()

          dch.ctx.strokeStyle = "red"
          dch.ctx.lineWidth = 5
          path.rotatedDirs.foreach { dir =>
            dch.ctx.beginPath()
            dch.ctx.moveTo(pos.x, pos.y)
            dch.ctx.lineTo(pos.x + dch.tileSize * dir.xOffs, pos.y + dch.screenTranslator.rowDistance * dir.yOffs)
            dch.ctx.stroke()
          }
        case _ =>
      }
    }

  implicit def tileMapDrawable(implicit td: Drawable[Tile], dch: Dch): Drawable[TileMap] =
    (self: TileMap, pos: ScreenPos) => {
      implicit val st = dch.screenTranslator
      self.tiles.foreach { case (pos, tile) =>
        td.draw(tile, pos.toScreen)
      }
    }

  implicit def gameStateDrawable(implicit tmd: Drawable[TileMap]): Drawable[GameState] =
    (self: GameState, pos: ScreenPos) => self.tileMap.drawTo(pos)

  implicit def overlayDrawable(implicit td: Drawable[Tile], dch: Dch): Drawable[GameOverlay] =
    (self: GameOverlay, pos: ScreenPos) => {
      implicit val st = dch.screenTranslator

      val overlayTile = self.gameState.nextTile.map(Tiles.VirtualTile).getOrElse(Tiles.Blank)
      td.draw(overlayTile, self.mousePos.toTile.toScreen)

      dch.ctx.beginPath()
      dch.ctx.arc(self.mousePos.x, self.mousePos.y, 10, 0, Math.PI * 2)
      dch.ctx.stroke()
    }

}