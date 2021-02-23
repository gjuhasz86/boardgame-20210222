package example
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Random

object Main {

  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    implicit val ctx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.canvas.width = dom.window.innerWidth.toInt
    ctx.canvas.height = dom.window.innerHeight.toInt
    println(dom.window.innerWidth)
    println(dom.window.innerHeight)

    val radius = 40
    val tileMap = TileMap(200, radius, radius, 14, 15)
    for (r <- 0 until tileMap.rows) {
      for (c <- 0 until tileMap.cols) {
        tileMap.drawTile(r, c, Tiles.Blank)
      }
    }
    def rndTile() = Tiles.Path(Random.nextInt(Tiles.Paths.rawAll.size - 1) + 1, 0).normalized
    var nextTile = Tiles.Path(0, 0)
    tileMap.drawTile(1, -2, nextTile)


    canvas.onmouseup = (e: dom.MouseEvent) =>
      tileMap.screenToTile(e.pageX.toInt, e.pageY.toInt).foreach { case (r, c) =>
        println(r, c, nextTile)
        tileMap.drawTileXy(e.pageX, e.pageY, nextTile)
        nextTile = rndTile()
        tileMap.drawTile(1, -2, nextTile)
        println(nextTile)
        println(nextTile.paths)
        println(nextTile.points)
      }
    canvas.onmousewheel = (e: dom.WheelEvent) => {
      nextTile = nextTile.copy(rot = nextTile.rot + e.deltaY.sign.toInt)
      tileMap.drawTile(1, -2, nextTile)
      println(nextTile)
      println(nextTile.paths)
      println(nextTile.points)
    }
  }

  implicit class DrawOnTileMap(val self: TileMap)(implicit ctx: dom.CanvasRenderingContext2D) {
    def drawTileXy(x: Double, y: Double, tile: Tile): Unit =
      self.screenToTile(x.toInt, y.toInt)
        .foreach { case (r, c) => drawTile(r, c, tile) }

    def drawTile(r: Int, c: Int, tile: Tile): Unit = {
      val (x, y) = self.tileToScreen(r, c)
      tile match {
        case Tiles.Blank =>
          ctx.beginPath()
          ctx.lineWidth = 1
          ctx.fillStyle = "white"
          ctx.strokeStyle = "black"
          ctx.arc(x, y, self.size, 0, 2 * Math.PI)
          ctx.fill()
          ctx.stroke()
        case Tiles.Green =>
          ctx.beginPath()
          ctx.lineWidth = 1
          ctx.fillStyle = "green"
          ctx.strokeStyle = "black"
          ctx.arc(x, y, self.size - 5, 0, 2 * Math.PI)
          ctx.fill()
          ctx.stroke()
        case tile@Tiles.Path(_, _) =>
          drawTile(r, c, Tiles.Blank)
          ctx.strokeStyle = "red"
          ctx.lineWidth = 5
          tile.points.foreach { case (fx, fy) =>
            ctx.beginPath()
            ctx.moveTo(x, y)
            ctx.lineTo(x + self.size * fx, y + self.distanceY * fy)
            ctx.stroke()
          }
      }
    }
  }


}

sealed trait Tile
object Tiles {
  case object Blank extends Tile
  case object Green extends Tile
  case class Path(pIdx: Int, rot: Int) extends Tile {
    def paths = Paths.all(pIdx)
    def points = paths.map(_ + rot).map(Paths.points)
    def normalized = Path(Math.abs(pIdx % Paths.rawAll.size), rot % Paths.rawPoints.size)
  }
  object Paths {
    def points(idx: Int) = rawPoints((idx % rawPoints.size + rawPoints.size) % rawPoints.size)
    val rawPoints = Vector(
      (0.5, -0.5),
      (1.0, 0.0),
      (0.5, 0.5),
      (-0.5, 0.5),
      (-1.0, 0.0),
      (-0.5, -0.5),
    )

    def all(idx: Int) = rawAll(Math.abs(idx % rawAll.size))
    val rawAll = Vector(
      Set(0, 1, 2, 3, 4, 5),
      //      Set(),
      //      Set(0),
      //      Set(0, 1), // 1,5
      //      Set(0, 2), // 2,4
      //      Set(0, 3), // 3,3
      Set(0, 1, 2), // 1,1,4
      Set(0, 1, 3), // 1,2,3
      Set(0, 1, 4), // 1,3,2
      Set(0, 2, 4), // 2,2,2
      //      Set(0, 1, 2, 3), // 1,1,1,3
      //      Set(0, 1, 2, 4), // 1,1,2,2
      //      Set(0, 1, 3, 4), // 1,2,1,2
      //      Set(0, 1, 2, 3, 4),
    )

  }
}

case class TileMap(topLeftX: Int, topLeftY: Int, size: Int, rows: Int, cols: Int) {
  val distanceY = Math.sqrt((size * size * 4) - size * size)
  def tileToScreen(r: Int, c: Int): (Int, Int) = {
    val offset = if (r % 2 == 0) 0 else size
    (topLeftX + offset + size * c * 2, (topLeftY + distanceY * r).toInt)
  }
  def screenToTile(x: Int, y: Int): Option[(Int, Int)] = {
    val r = Math.round((y - topLeftY) / distanceY).toInt
    val offset = if (r % 2 == 0) 0 else size
    val c = Math.round((x - topLeftX - offset) / (size.toDouble * 2)).toInt
    if (r < rows && c < cols && r >= 0 && c >= 0) Some((r, c)) else None
  }
}
