package undertale.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import undertale.logic.UndertaleLogic._

import scala.collection.mutable

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class UndertaleLogic(val randomGen: RandomGenerator,
                     val gridDims: Dimensions,
                     val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  // player vs sans
  var onPlayer: Player = Player(gridDims)
  val onSans: Sans = Sans(gridDims, randomGen)

  // effect flags
  var positionModified : Boolean = false

  // attack frames, FIFO
  var frameQueue: mutable.Queue[Frame] = new mutable.Queue[Frame]()

  def outOfBoundModify(player : Player): Player = {
    val p = player.position
    if ((p.x > 0) && (p.x < gridDims.width - 1) && (p.y > 0) && (p.y < gridDims.height - 1)) {
      player
    } else {
      positionModified = true
      player.copy(position = Point(gridDims.width / 2, gridDims.height / 2))
    }
  }

  // player control
  def moveUp(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveUp())

  def moveDown(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveDown())

  def moveLeft(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveLeft())

  def moveRight(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveRight())

  def moveUpLeft(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveUp().moveLeft())

  def moveUpRight(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveUp().moveRight())

  def moveDownLeft(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveDown().moveLeft()
)
  def moveDownRight(): Unit =
    onPlayer = outOfBoundModify(onPlayer.moveDown().moveRight())

  var damageFlash: Boolean = false

  def checkDmg(): Unit = {
    if (currentFrame.isEmpty) return

    onPlayer.getHitbox().foreach(e => {
      val x = e.x
      val y = e.y
      if (currentFrame(x)(y).isInstanceOf[DamageCell] && onPlayer.damageImmune == 0) {
        onPlayer = onPlayer.takeDmg()
        onPlayer = onPlayer.resetImmune()
        damageFlash = true
      }
    })
  }

  def cheatMode(): Unit = {
    onPlayer = onPlayer.copy(health = 500)
  }


  // Graphic control
  def currentFrame: List[List[CellType]] = if (frameQueue.nonEmpty) frameQueue.front.graphic else List[List[CellType]]()

  def step(): Unit = {
    if (frameQueue.isEmpty) {
      // choose random dialog
      // create next attack wave, enqueue
      frameQueue = onSans.attack()
    } else {
      // dequeue next frame
      frameQueue.dequeue()
      checkDmg()
      if (onPlayer.damageImmune > 0) onPlayer = onPlayer.reduceImmune()
    }
  }

  def isGameOver: Boolean = {
    if (onPlayer.health <= 0) true
    else false
  }

  //
  def getCellType(p: Point): CellType = {
    if (p == onPlayer.position) return Soul

    try {
      if (frameQueue.nonEmpty) {
        // data structured as a list of Columns
        return frameQueue.front.graphic(p.x)(p.y)
      }
    } catch {
      case e: IndexOutOfBoundsException => {
        return Empty
      }
    }

    Empty
  }

}

object UndertaleLogic {

  val FramesPerSecond: Int = 60 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller


  def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 120
  val NrTopInvisibleLines: Int = 30
  val DefaultVisibleHeight: Int = 30
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new UndertaleLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}