package hexgrid.core

case class Monster(level: Int, power: Int, owner: Player) {
  def incPower: Monster = copy(power = power + 1)
  def incLevel: Monster = copy(level = level + 1)
  def decPower: Monster = copy(power = power - 1)
  def decLevel: Monster = copy(level = level - 1)
  def clearPower: Monster = copy(power = 0)
  def levelUp: Monster = if (power >= 6) this.clearPower.incLevel else this
}