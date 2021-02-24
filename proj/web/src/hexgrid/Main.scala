package hexgrid
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    import Drawable._
    import Drawables._

    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    implicit val dch = new DrawContextHolder[dom.CanvasRenderingContext2D] {
      override def ctx: CanvasRenderingContext2D = renderCtx
    }
    var overlay = GameOverlay(ScreenPos(0, 0))

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      overlay.drawTo(ScreenPos(0, 0))
    }

    canvas.onmousemove = (e: dom.MouseEvent) => {
      overlay = overlay.setMouse(ScreenPos(e.pageX, e.pageY))
      dom.window.requestAnimationFrame(updateScreen)
    }
  }

}

case class TilePos(r: Int, c: Int)
case class ScreenPos(x: Int, y: Int)
object ScreenPos {
  def apply(x: Double, y: Double): ScreenPos = new ScreenPos(x.toInt, y.toInt)
}

sealed trait Tile
case object Blank extends Tile
case object Green extends Tile
case object Red extends Tile

case class TileMap(tiles: Map[TilePos, Tile]) {
  def place(pos: TilePos, tile: Tile): TileMap =
    copy(tiles = tiles + (pos -> tile))
}
case class GameState(tileMap: TileMap)
case class GameOverlay(mousePos: ScreenPos) {
  def setMouse(pos: ScreenPos) = copy(mousePos = pos)
}


trait DrawContextHolder[T] {
  def ctx: T
}

trait Drawable[T] {
  def draw(self: T, pos: ScreenPos): Unit
}
object Drawable {
  implicit class DrawOnDrawable[T](self: T)(implicit d: Drawable[T]) {
    def drawTo(pos: ScreenPos) = d.draw(self, pos)
  }
}
object Drawables {
  //  import Drawable._

  implicit def tileMapDrawable(implicit td: Drawable[Tile]): Drawable[TileMap] = new Drawable[TileMap] {
    override def draw(self: TileMap, pos: ScreenPos): Unit = {
      self.tiles.foreach { case (pos, tile) =>
        ???
      }
    }
  }

  implicit def overlayDrawable(implicit dch: DrawContextHolder[dom.CanvasRenderingContext2D]): Drawable[GameOverlay] =
    (self: GameOverlay, pos: ScreenPos) => {
      dch.ctx.beginPath()
      dch.ctx.arc(self.mousePos.x, self.mousePos.y, 10, 0, Math.PI * 2)
      dch.ctx.stroke()
    }

}