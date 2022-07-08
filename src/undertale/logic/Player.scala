package undertale.logic


case class Player(health: Int,
                  position: Point,
                  jumpStamina: Int,
                  damageImmune: Int) {
  def moveUp(): Player =
    copy(position = Point(this.position.x, this.position.y - 1))

  def moveDown(): Player =
    copy(position = Point(this.position.x, this.position.y + 1))

  def moveLeft(): Player =
    copy(position = Point(this.position.x - 1, this.position.y))

  def moveRight(): Player =
    copy(position = Point(this.position.x + 1, this.position.y))

  def takeDmg(): Player =
    copy(health = this.health - 1)

  def reduceImmune(): Player =
    copy(damageImmune = this.damageImmune - 1)

  def resetImmune(): Player =
    copy(damageImmune = 10)

  def getHitbox(): List[Point] = {
    val x = position.x
    val y = position.y

    List[Point](
      position,
      Point(x + 1, y),
      Point(x - 1, y),
      Point(x, y + 1),
      Point(x, y - 1),
      Point(x + 1, y + 1),
      Point(x - 1, y - 1),
    )
  }

}

object Player {
  def apply(gridDims: Dimensions): Player = {
    val x: Int = gridDims.width / 2
    val y: Int = gridDims.height / 2

    Player(10, Point(x, y), 4, 0)
  }
}