package hexgrid
import hexgrid.core.TilePos

sealed trait GameAction
object GameAction {
  case object Confirm extends GameAction
  case object Cancel extends GameAction
  case object Move extends GameAction
  case class SelectMonster(pos: TilePos) extends GameAction
  case class SelectMonsterTarget(pos: TilePos) extends GameAction
  case object DrawTile extends GameAction
  case class SelectTileTarget(pos: TilePos) extends GameAction
  case object RotateTileLeft extends GameAction
  case object RotateTileRight extends GameAction
  case object Noop extends GameAction
}

