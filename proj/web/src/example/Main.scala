package example
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    implicit val ctx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.canvas.width = dom.window.innerWidth.toInt
    ctx.canvas.height = dom.window.innerHeight.toInt
    println(dom.window.innerWidth)
    println(dom.window.innerHeight)

    val radius = 50
    val tileMap = TileMap(radius, 10, 10)
    for (r <- 0 until tileMap.rows) {
      for (c <- 0 until tileMap.cols) {
        val (x, y) = tileMap.tileToScreen(r, c)
        ctx.beginPath()
        ctx.arc(x, y, radius, 0, 2 * Math.PI)
        ctx.stroke()
      }
    }

    canvas.onmousedown = (e: dom.MouseEvent) => {
      tileMap.drawTileXy(e.pageX, e.pageY, Tiles.Green)
    }
    canvas.onmouseup = (e: dom.MouseEvent) => {
      tileMap.drawTileXy(e.pageX, e.pageY, Tiles.Blank)
    }
  }

  implicit class DrawOnTileMap(val self: TileMap)(implicit ctx: dom.CanvasRenderingContext2D) {
    def drawTileXy(x: Double, y: Double, tile: Tile): Unit =
      self.screenToTile(x.toInt, y.toInt)
        .foreach { case (r, c) => drawTile(r, c, tile) }

    def drawTile(r: Int, c: Int, tile: Tile): Unit = {
      val (x, y) = self.tileToScreen(r, c)
      ctx.beginPath()
      tile match {
        case Tiles.Blank =>
          ctx.fillStyle = "white"
          ctx.strokeStyle = "black"
          ctx.arc(x, y, self.size, 0, 2 * Math.PI)
        case Tiles.Green =>
          ctx.fillStyle = "green"
          ctx.strokeStyle = "black"
          ctx.arc(x, y, self.size - 5, 0, 2 * Math.PI)
      }
      ctx.fill()
      ctx.stroke()
    }
  }


}

sealed trait Tile
object Tiles {
  case object Blank extends Tile
  case object Green extends Tile
}

case class TileMap(size: Int, rows: Int, cols: Int) {
  val topLeftX = size;
  val topLeftY = size;
  val distanceY = Math.sqrt(size * size + (size * size * 4))
  def tileToScreen(r: Int, c: Int): (Int, Int) = {
    val offset = if (r % 2 == 0) 0 else size
    (topLeftX + offset + size * c * 2, (topLeftY + 86.60254 * r).toInt)
  }
  def screenToTile(x: Int, y: Int): Option[(Int, Int)] = {
    val r = Math.round((y - topLeftY) / 86.60254).toInt
    val offset = if (r % 2 == 0) 0 else size
    val c = Math.round((x - topLeftX - offset) / (size.toDouble * 2)).toInt
    if (r < rows && c < cols) Some((r, c)) else None
  }
}
