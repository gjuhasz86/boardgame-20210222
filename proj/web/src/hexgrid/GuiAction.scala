package hexgrid

sealed trait GuiAction
object GuiAction {
  case object Click extends GuiAction
  case class Key(code: Int) extends GuiAction
}