package hexgrid

import hexgrid.core.Cards
import hexgrid.core.Dirs
import hexgrid.core.GameState
import hexgrid.core.TilePos
import hexgrid.gui.DrawContext
import hexgrid.gui.MapOffsetAwareScreenTranslator
import hexgrid.gui.ScreenPos
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._
import org.scalajs.dom.ext.KeyCode


sealed trait GuiPending
object GuiPending {
  case class MapMove(offset: ScreenPos) extends GuiPending
  case object NoPending extends GuiPending
}

class GameManager(
  var state: GameState, var phase: GamePhase, var guiPending: GuiPending, var mapOffset0: ScreenPos, var debug: Boolean,
  drawContext: DrawContext, st: ScreenTranslator) {

  import GameAction._
  import GamePhase._
  import GuiAction._
  import GuiPending._

  private implicit val dc: DrawContext = drawContext
  implicit val screenTranslator: MapOffsetAwareScreenTranslator = new MapOffsetAwareScreenTranslator {
    override def innerSt: ScreenTranslator = st
    override def mapOffset: ScreenPos = mapOffsetAccessor
  }
  private def mapOffsetAccessor = mapOffset

  def mapOffset: ScreenPos = guiPending match {
    case MapMove(pos) => mapOffset0 + dc.cursorPos - pos
    case _ => mapOffset0
  }

  def tryPerform(action: GuiAction): Boolean = {
    val gameAction = toGameAction(action)
    println(action, gameAction)
    changeGui(action)
    tryPerform(gameAction)
  }


  def changeGui(action: GuiAction): Unit =
    guiPending =
      (action, guiPending) match {
        case (RightClickDown, _) =>
          println(state.validTiles(dc.cursorPos.toTile))
          println(dc.cursorPos.toTile.ring(1).flatMap(state.tileAt))
          MapMove(dc.cursorPos)
        case (RightClickUp, MapMove(pos)) =>
          mapOffset0 += dc.cursorPos - pos
          NoPending
        case (Key(KeyCode.B), _) =>
          debug = !debug
          guiPending
        case _ =>
          guiPending
      }

  def toGameAction(action: GuiAction): GameAction = {
    val pos = dc.cursorPos.toTile
    (action, phase) match {
      case (Click, Idle) if dc.tileStackPos.distanceTo(dc.cursorPos) < dc.tileSize =>
        DrawTile
      case (Click, Idle) if state.monsters.tiles.get(pos).isDefined =>
        SelectMonster(pos)
      case (Key(KeyCode.D), Idle) =>
        DrawTile
      case (Key(KeyCode.M), Idle) =>
        Move
      case (Click, MoveMonster(None, _)) =>
        SelectMonster(pos)
      case (Click, MoveMonster(Some(_), _)) =>
        SelectMonsterTarget(pos)
      case (Click, PlacingNextTile(_)) =>
        SelectTileTarget(pos)
      case (Key(KeyCode.E), _) if isPlacingTile =>
        RotateTileRight
      case (Key(KeyCode.Q), _) if isPlacingTile =>
        RotateTileLeft
      case (WheelDown, _) if isPlacingTile =>
        RotateTileRight
      case (WheelUp, _) if isPlacingTile =>
        RotateTileLeft
      case (Key(KeyCode.Backspace), _) =>
        Cancel
      case (Key(KeyCode.Enter), _) =>
        Confirm
      case (Key(KeyCode.Space), _) =>
        Confirm
      case _ =>
        Noop

    }
  }

  def tryPerform(action: GameAction): Boolean =
    if (isValid(action)) {
      perform(action)
      true
    } else {
      false
    }

  def perform(action: GameAction): Unit =
    action match {
      case Move =>
        phase = MoveMonster(None, None)
      case SelectMonster(pos) =>
        phase = MoveMonster(Some(pos), None)
      case SelectMonsterTarget(pos) =>
        phase match {
          case MoveMonster(from, _) =>
            phase = MoveMonster(from, Some(pos))
            perform(Confirm)
          case _ =>
        }
      case DrawTile =>
        state = state.revealNextTile
        state.winner match {
          case Some(p) =>
            phase = GameOver(p, noValidPlacement = true)
          case _ =>
            phase = PlacingNextTile(None)
        }
      case SelectTileTarget(pos) =>
        phase = PlacingNextTile(Some(pos))
        perform(Confirm)
      case RotateTileLeft =>
        state = state.changeNextTile(_.rotateLeft)
      case RotateTileRight =>
        state = state.changeNextTile(_.rotateRight)
      case Confirm =>
        phase match {
          case PlacingNextTile(Some(pos)) =>
            state = state.placeNext(pos)
            if (state.nextCard == Some(Cards.PlaceBlob)) {
              state = state.placeBlob(pos)
            }
            state = state.drawCard.endTurn
            phase = Idle
          case MoveMonster(Some(from), Some(to)) =>
            state = state.moveMonster(from, to)
            if (state.blobs.contains(to)) {
              state = state.takeBlob(to).changeMonster(to)(_.incPower.levelUp)
            }
            state = state.endTurn
            phase = state.winner match {
              case Some(p) => GameOver(p)
              case _ => Idle
            }
          case _ =>
        }
      case Cancel if isPlacingTile =>
        phase = PlacingNextTile(None)
      case Cancel =>
        phase = Idle
      case Noop =>
    }

  def isValid(action: GameAction): Boolean = {
    (action, phase) match {
      case (Move, PlacingNextTile(_)) => false
      case (Move, _) => true
      case (SelectMonster(pos), Idle | MoveMonster(_, _)) =>
        state.monsters.tiles.get(pos).exists(_.owner == state.nextPlayer)
      case (SelectMonster(_), _) => false
      case (SelectMonsterTarget(to), MoveMonster(Some(from), _)) =>
        state.tileMap.tiles.get(from).isDefined && state.canMove(from, to)
      case (SelectMonsterTarget(_), _) => false
      case (DrawTile, PlacingNextTile(_)) => false
      case (DrawTile, _) => state.nextTile.isDefined
      case (SelectTileTarget(pos), PlacingNextTile(_)) => canPlaceTile(pos)
      case (SelectTileTarget(_), _) => false
      case (RotateTileLeft, _) if isPlacingTile => true
      case (RotateTileLeft, _) => false
      case (RotateTileRight, _) if isPlacingTile => true
      case (RotateTileRight, _) => false
      case (Confirm, MoveMonster(Some(_), Some(_))) => true
      case (Confirm, PlacingNextTile(Some(_))) => true
      case (Confirm, _) => false
      case (Cancel, PlacingNextTile(None)) => false
      case (Cancel, _) => true
      case (_, GameOver(_, _)) => false
      case (Noop, _) => false
    }
  }


  def canPlaceTile(pos: TilePos): Boolean = {
    val isPosEmpty = state.tileMap.tiles.get(pos).isEmpty
    val canJoin = state.nextTile match {
      case Some(tile) =>
        val neighbors = Dirs.all.flatMap(dir => state.tileMap.tiles.get(pos.neighbor(dir)).map(dir -> _))
        neighbors.nonEmpty &&
          neighbors.forall { case (d, t) => tile.canPlaceNextTo(t, d) } &&
          neighbors.exists { case (d, t) => tile.isJoined(t, d) }
      case None => false
    }
    val hasNearMonster =
      state.monsters.tiles.exists { case (mpos, m) =>
        mpos.distanceTo(pos) <= 3 && m.owner == state.nextPlayer
      }

    isPosEmpty && canJoin && hasNearMonster
  }

  def isPlacingTile: Boolean = phase match {
    case PlacingNextTile(_) => true
    case GameOver(_, _) => true
    case _ => false
  }

  def hasPlacedTile: Boolean = phase match {
    case PlacingNextTile(Some(_)) => true
    case _ => false
  }
}

object GameManager {
  def apply(drawContext: DrawContext, st: ScreenTranslator): GameManager =
    new GameManager(GameState.default(), GamePhase.Idle, GuiPending.NoPending, ScreenPos(0, 0), false, drawContext, st)
}