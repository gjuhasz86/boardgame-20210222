package hexgrid

import hexgrid.core.GameState
import hexgrid.gui.DrawContext
import hexgrid.gui.ScreenTranslator
import hexgrid.gui.ScreenTranslator._
import org.scalajs.dom.ext.KeyCode


class GameManager(var state: GameState, var phase: GamePhase, drawContext: DrawContext) {
  import GameAction._
  import GamePhase._
  import GuiAction._

  private implicit val dc: DrawContext = drawContext
  private implicit val st: ScreenTranslator = drawContext.screenTranslator

  def tryPerform(action: GuiAction): Boolean = {
    println(action)
    val gameAction = toGameAction(action)
    println(gameAction)
    tryPerform(gameAction)
  }

  def toGameAction(action: GuiAction): GameAction = {
    val pos = dc.cursorPos.toTile
    (action, phase) match {
      case (Click, Idle) if dc.tileStackPos.distanceTo(dc.cursorPos) < dc.tileSize =>
        DrawTile
      case (Key(KeyCode.M), Idle) =>
        Move
      case (Click, MoveMonster(None, _)) =>
        SelectMonster(pos)
      case (Click, MoveMonster(Some(_), _)) =>
        SelectMonsterTarget(pos)
      case (Click, PlacingNextTile(_)) =>
        SelectTileTarget(pos)
      case (Key(KeyCode.E), PlacingNextTile(_)) =>
        RotateTileRight
      case (Key(KeyCode.Q), PlacingNextTile(_)) =>
        RotateTileLeft
      case (Key(KeyCode.Backspace), _) =>
        Cancel
      case (Key(KeyCode.Enter), _) =>
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
        phase = phase match {
          case MoveMonster(from, _) => MoveMonster(from, Some(pos))
          case _ => phase
        }
      case DrawTile =>
        phase = PlacingNextTile(None)
      case SelectTileTarget(pos) =>
        phase = PlacingNextTile(Some(pos))
      case RotateTileLeft =>
        state = state.changeNextTile(_.rotateLeft)
      case RotateTileRight =>
        state = state.changeNextTile(_.rotateRight)
      case Confirm =>
        phase match {
          case PlacingNextTile(Some(pos)) =>
            state = state.placeNext(pos)
            phase = Idle
          case MoveMonster(Some(from), Some(to)) =>
            state = state.moveMonster(from, to)
            phase = Idle
          case _ =>
        }
      case Cancel if isPlacingTile =>
        phase = PlacingNextTile(None)
      case Cancel =>
        Idle
      case Noop =>
    }

  def isValid(action: GameAction): Boolean = {
    (action, phase) match {
      case (Move, PlacingNextTile(_)) => false
      case (Move, _) => true
      case (SelectMonster(pos), MoveMonster(_, _)) => state.monsters.tiles.get(pos).isDefined
      case (SelectMonster(_), _) => false
      case (SelectMonsterTarget(to), MoveMonster(Some(from), _)) =>
        state.tileMap.tiles.get(from).isDefined && from != to
      case (SelectMonsterTarget(_), _) => false
      case (DrawTile, PlacingNextTile(_)) => false
      case (DrawTile, _) => true
      case (SelectTileTarget(pos), PlacingNextTile(_)) => state.tileMap.tiles.get(pos).isEmpty
      case (SelectTileTarget(_), _) => false
      case (RotateTileLeft, PlacingNextTile(_)) => true
      case (RotateTileLeft, _) => false
      case (RotateTileRight, PlacingNextTile(_)) => true
      case (RotateTileRight, _) => false
      case (Confirm, MoveMonster(Some(_), Some(_))) => true
      case (Confirm, PlacingNextTile(Some(_))) => true
      case (Confirm, _) => false
      case (Cancel, PlacingNextTile(None)) => false
      case (Cancel, _) => true
      case (Noop, _) => false
    }
  }

  def isPlacingTile: Boolean = phase match {
    case PlacingNextTile(_) => true
    case _ => false
  }
}

object GameManager {
  def apply(drawContext: DrawContext): GameManager = new GameManager(GameState.default(), GamePhase.Idle, drawContext)
}