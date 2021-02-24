package hexgrid

case class GameOverlay(mousePos: ScreenPos, gameState: GameState) {
  def setMouse(pos: ScreenPos) = copy(mousePos = pos)
  def changeState(f: GameState => GameState) = copy(gameState = f(gameState))
}