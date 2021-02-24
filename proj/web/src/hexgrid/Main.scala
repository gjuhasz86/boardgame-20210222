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
      override def tileSize: Int = 50
      override def screenTranslator: ScreenTranslator = DefaultScreenTranslator(canvas.width, canvas.height, tileSize)
    }
    var overlay = GameOverlay(ScreenPos(0, 0))
    val tileMap =
      TileMap.empty
        .place(0, 0, Tiles.Green)
        .place(0, 1, Tiles.Blank)
        .place(0, 2, Tiles.Green)
        .place(0, 3, Tiles.Blank)

    var gameState = GameState(tileMap)

    implicitly[Drawable[TileMap]]

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      gameState.drawTo(ScreenPos(0, 0))
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
object Tiles {
  case object Blank extends Tile
  case object Green extends Tile
  case object Red extends Tile
}

case class TileMap(tiles: Map[TilePos, Tile]) {
  def place(r: Int, c: Int, tile: Tile): TileMap =
    place(TilePos(r, c), tile)

  def place(pos: TilePos, tile: Tile): TileMap =
    copy(tiles = tiles + (pos -> tile))
}

object TileMap {
  def empty = TileMap(Map.empty)
}

case class GameState(tileMap: TileMap)
object GameState {
  def apply(): GameState = new GameState(TileMap(Map.empty))
}


case class GameOverlay(mousePos: ScreenPos) {
  def setMouse(pos: ScreenPos) = copy(mousePos = pos)
}


trait DrawContextHolder[T] {
  def ctx: T
  def tileSize: Int
  def screenTranslator: ScreenTranslator
}

trait ScreenTranslator {
  def tileToScreen(pos: TilePos): ScreenPos
  def screenToTile(pos: ScreenPos): TilePos
}
object ScreenTranslator {
  implicit class ToScreenOnTilePos(self: TilePos)(implicit st: ScreenTranslator) {
    def toScreen: ScreenPos = st.tileToScreen(self)
  }

  implicit class ToTileOnScreenPos(self: ScreenPos)(implicit st: ScreenTranslator) {
    def toTile: TilePos = st.screenToTile(self)
  }
}

case class DefaultScreenTranslator(screenWidth: Int, screenHeight: Int, size: Int) extends ScreenTranslator {
  val distanceY = Math.sqrt((size * size * 4) - size * size)
  val origin = ScreenPos(screenWidth / 2, screenHeight / 2)

  override def tileToScreen(pos: TilePos): ScreenPos = {
    val offset = if (pos.r % 2 == 0) 0 else size
    ScreenPos(origin.x + offset + size * pos.c * 2, (origin.y + distanceY * pos.r).toInt)
  }

  override def screenToTile(pos: ScreenPos): TilePos = {
    val r = Math.round((pos.y - origin.y) / distanceY).toInt
    val offset = if (r % 2 == 0) 0 else size
    val c = Math.round((pos.x - origin.x - offset) / (size.toDouble * 2)).toInt
    TilePos(r, c)
  }
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
    (self: Tile, pos: ScreenPos) =>
      self match {
        case Tiles.Blank =>
          dch.ctx.beginPath()
          dch.ctx.arc(pos.x, pos.y, dch.tileSize, 0, Math.PI * 2)
          dch.ctx.fillStyle = "white"
          dch.ctx.fill()
          dch.ctx.stroke()
        case Tiles.Green =>
          dch.ctx.beginPath()
          dch.ctx.arc(pos.x, pos.y, dch.tileSize, 0, Math.PI * 2)
          dch.ctx.fillStyle = "green"
          dch.ctx.fill()
          dch.ctx.stroke()
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

  implicit def overlayDrawable(implicit dch: Dch): Drawable[GameOverlay] =
    (self: GameOverlay, pos: ScreenPos) => {
      dch.ctx.beginPath()
      dch.ctx.arc(self.mousePos.x, self.mousePos.y, 10, 0, Math.PI * 2)
      dch.ctx.stroke()
    }

}