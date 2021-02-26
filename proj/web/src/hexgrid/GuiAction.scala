package hexgrid

sealed trait GuiAction
object GuiAction {
  case object Click extends GuiAction
  case object RightClickDown extends GuiAction
  case object RightClickUp extends GuiAction
  case class Key(code: Int) extends GuiAction
  case object WheelUp extends GuiAction
  case object WheelDown extends GuiAction
}