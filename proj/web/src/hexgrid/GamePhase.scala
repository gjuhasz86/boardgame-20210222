package hexgrid
import hexgrid.core.Player
import hexgrid.core.TilePos

sealed trait GamePhase
object GamePhase {
  case class MoveMonster(from: Option[TilePos], to: Option[TilePos]) extends GamePhase
  case class PlacingNextTile(pos: Option[TilePos]) extends GamePhase
  case object Idle extends GamePhase
  case class GameOver(winner: Player, noValidPlacement: Boolean = false) extends GamePhase

  implicit class HelpersOnGamePhase(self: GamePhase) {

    def isPlacingTile: Boolean = self match {
      case PlacingNextTile(_) => true
      case _ => false
    }

  }
}