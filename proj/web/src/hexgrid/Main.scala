package hexgrid
import hexgrid.Tiles.GameTile
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Random

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
sealed abstract class Dir(val idx: Int, val xOffs: Double, val yOffs: Double) {
  def rotate(n: Int): Dir = {
    val count = Dirs.all.size
    Dirs.byIdx(((n + idx) % count + count) % count)
  }

  def rotateRight: Dir = rotate(1)
  def rotateLeft: Dir = rotate(-1)
}

object Dirs {
  case object UR extends Dir(0, 0.5, -0.5)
  case object RR extends Dir(1, 1.0, 0.0)
  case object DR extends Dir(2, 0.5, 0.5)
  case object DL extends Dir(3, -0.5, 0.5)
  case object LL extends Dir(4, -1.0, 0.0)
  case object UL extends Dir(5, -0.5, -0.5)

  val all: Seq[Dir] = List(UR, RR, DR, DL, LL, UL).sortBy(_.idx)
  def byIdx(idx: Int): Dir = all.find(_.idx == idx).get
}

sealed trait Tile {
  def rotate(n: Int): Tile
  def rotateRight: Tile = rotate(1)
  def rotateLeft: Tile = rotate(-1)
}
object Tiles {
  case object Blank extends Tile {
    override def rotate(n: Int): Tile = this
  }
  case class VirtualTile(inner: GameTile) extends Tile {
    override def rotate(n: Int): Tile = copy(inner.rotate(n))
  }

  sealed trait GameTile extends Tile {
    override def rotate(n: Int): GameTile
    override def rotateRight: GameTile = rotate(1)
    override def rotateLeft: GameTile = rotate(-1)
  }

  case class Path private(dirs: Set[Dir], rot: Int) extends GameTile {
    override def rotate(n: Int): Path = copy(rot = rot + n)
    def rotatedDirs: Set[Dir] = dirs.map(_.rotate(rot))
  }

  import Dirs._

  val E = Path(Set(UR, RR, DR), 0)
  val Lambda = Path(Set(UL, DL, DR), 0)
  val Y = Path(Set(UL, UR, DR), 0)
  val Center = Path(Set(UR, DR, LL), 0)

  val gameTiles = List(E, Lambda, Y, Center)

}

case class TileMap(tiles: Map[TilePos, GameTile]) {
  def place(r: Int, c: Int, tile: GameTile): TileMap =
    place(TilePos(r, c), tile)

  def place(pos: TilePos, tile: GameTile): TileMap =
    copy(tiles = tiles + (pos -> tile))
}

object TileMap {
  def empty = TileMap(Map.empty)
}

case class GameState(tileMap: TileMap, tileStack: List[GameTile]) {
  def placeNext(pos: TilePos) = tileStack match {
    case tile :: rest => place(pos, tile).copy(tileStack = rest)
    case _ => this
  }

  def place(pos: TilePos, tile: GameTile): GameState = copy(tileMap = tileMap.place(pos, tile))
  def nextTile: Option[GameTile] = tileStack.headOption
  def changeNextTile(f: GameTile => GameTile): GameState = tileStack match {
    case tile :: rest => copy(tileStack = f(tile) :: rest)
    case _ => this
  }
}

object GameState {
  def random(): GameState = {
    val tileStack = Random.shuffle(List.fill(25)(Tiles.gameTiles).flatten)
    new GameState(TileMap.empty, tileStack)
  }
}


case class GameOverlay(mousePos: ScreenPos, gameState: GameState) {
  def setMouse(pos: ScreenPos) = copy(mousePos = pos)
  def changeState(f: GameState => GameState) = copy(gameState = f(gameState))
}


trait DrawContextHolder[T] {
  def ctx: T
  def tileSize: Int
  def screenTranslator: ScreenTranslator
}

trait ScreenTranslator {
  def rowDistance: Double
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
  val rowDistance = Math.sqrt((size * size * 4) - size * size)
  val origin = ScreenPos(screenWidth / 2, screenHeight / 2)

  override def tileToScreen(pos: TilePos): ScreenPos = {
    val offset = if (pos.r % 2 == 0) 0 else size
    ScreenPos(origin.x + offset + size * pos.c * 2, (origin.y + rowDistance * pos.r).toInt)
  }

  override def screenToTile(pos: ScreenPos): TilePos = {
    val r = Math.round((pos.y - origin.y) / rowDistance).toInt
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