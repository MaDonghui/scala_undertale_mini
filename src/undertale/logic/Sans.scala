package undertale.logic

import engine.random.RandomGenerator

import scala.collection.mutable

case class Sans(val gridDims: Dimensions,
                val randomGen: RandomGenerator) {
  // figure can wait

  //
  val emptyBoard: List[List[CellType]] = {
    val emptyColumn: List[CellType] = List.fill(gridDims.height)(Empty)
    List.fill(gridDims.width)(emptyColumn)
  }

  // attacks
  // the reason I choose to use underline style is to improve readability
  // different move different
  def attack(): mutable.Queue[Frame] = {
    val index: Int = randomGen.randomInt(5)

    index match {
      case 0 => attack_bone_wave
      case 1 => attack_bone_gravity
      case 2 => attack_bone_jump
      case 3 => attack_laser_square
      case 4 => attack_laser_multiple
      case _ => mutable.Queue[Frame]()
    }
  }

  def attack_bone_wave: mutable.Queue[Frame] = {
    val res: mutable.Queue[Frame] = new mutable.Queue[Frame]
    res.enqueue(Frame(emptyBoard, List[String]()))

    val maxMove: Int = 2
    val maxHeight: Int = gridDims.height / 2 - maxMove
    val maxLength: Int = gridDims.width * 6

    val initBoneCells: List[CellType] = {
      var columnBuffer: List[CellType] = List[CellType]()
      for (i <- 0 until (maxHeight / 2) - 1) columnBuffer = columnBuffer :+ BoneVerticalMid :+ BoneDamageBlock
      columnBuffer = columnBuffer :+ BoneVerticalBot
      for (i <- 0 until maxMove * 3) columnBuffer = columnBuffer :+ Empty
      columnBuffer = columnBuffer :+ BoneVerticalTop
      for (i <- 0 until (maxHeight / 2) - 1) columnBuffer = columnBuffer :+ BoneVerticalMid :+ BoneDamageBlock

      columnBuffer
    }

    var columnBuffer: List[CellType] = initBoneCells
    var currMove: Int = 0
    // var nonLinearChangeFactor: Int = 0
    var currDirection: Boolean = true
    // every third column, update position, else fill in invisible bone damage block
    for (w <- 0 until maxLength) {
      // duo to image size, add bone image ever 3rd column
      if (w % 4 == 0) {
        currDirection match {
          case true => {
            columnBuffer = columnBuffer.tail :+ BoneVerticalMid
            currMove = currMove + 1
          }
          case false => {
            columnBuffer = BoneVerticalMid +: columnBuffer.dropRight(1)
            currMove = currMove - 1
          }
        }
        // compose frame
        res.enqueue(Frame(res.last.graphic.tail :+ columnBuffer, List[String]()))
        // reverse move direction
        if (currMove > maxMove * 2 || currMove < -maxMove * 2) currDirection = !currDirection
      } else {
        // add invisible bone damage area
        var invisibleColumn: List[CellType] =
          for (cell <- columnBuffer) yield if (cell.isInstanceOf[Bone]) BoneDamageBlock else cell
        res.enqueue(Frame(res.last.graphic.tail :+ invisibleColumn, List[String]()))
      }
    }

    res
  }

  def attack_bone_gravity: mutable.Queue[Frame] = {
    val res: mutable.Queue[Frame] = mutable.Queue[Frame]()
    // out of time
    res
  }

  def attack_bone_jump: mutable.Queue[Frame] = {
    val res: mutable.Queue[Frame] = mutable.Queue[Frame]()
    // not enough time
    res
  }

  def attack_laser_multiple: mutable.Queue[Frame] = {
    var res: mutable.Queue[Frame] = mutable.Queue[Frame]()

    for (i <- 0 until randomGen.randomInt(6)) {
      attack_laser_rectangle.foreach(e => {
        res.enqueue(e)
      })
    }

    res
  }

  def attack_laser_square: mutable.Queue[Frame] = {
    val res: mutable.Queue[Frame] = mutable.Queue[Frame]()
    // don't judge
    res
  }

  def attack_laser_rectangle: mutable.Queue[Frame] = {
    val res: mutable.Queue[Frame] = mutable.Queue[Frame]()

    var laserRows: List[Int] = List[Int]()

    // first round, horizontal width / 6 laser left right
    def initialFire(): (List[CellType], List[CellType]) = {
      var bufferLeft: List[CellType] = List[CellType]()
      var bufferRight: List[CellType] = List[CellType]()
      for (i <- 0 until gridDims.height) {
        if (randomGen.randomInt(30) < 3) {
          bufferLeft = bufferLeft :+ LaserHeadLeft
          bufferRight = bufferRight :+ LaserHeadRight
          laserRows = laserRows :+ i :+ (i - 1)
        } else {
          bufferLeft = bufferLeft :+ Empty
          bufferRight = bufferRight :+ Empty
        }
      }

      (bufferLeft, bufferRight)
    }

    val laser_left: List[CellType] = initialFire()._1
    val laser_right: List[CellType] = initialFire()._2

    var weaponWarmedUp: List[List[CellType]] = List[List[CellType]]()
    // weapon appear and warm up
    for (i <- 1 until 6) {
      // weapon pushing
      var frameBuffer: List[List[CellType]] = emptyBoard.dropRight(i).drop(i)

      frameBuffer = laser_right +: frameBuffer
      frameBuffer = frameBuffer :+ laser_left

      for (x <- 1 until i) {
        frameBuffer = List.fill(gridDims.height)(Empty) +: frameBuffer
        frameBuffer = frameBuffer :+ List.fill(gridDims.height)(Empty)
      }

      weaponWarmedUp = frameBuffer
      // pause for response and wait for fire, first frame has sound
      if (i == 1) res.enqueue(Frame(frameBuffer, List[String]("laser")))
      if (i == 5) {
        for (x <- 0 until 30) res.enqueue(Frame(frameBuffer, List[String]()))
      }
    }

    // FIRE
    var columnBuffer: List[CellType] = List[CellType]()
    for (x <- 0 until gridDims.height) {
      if (laserRows.contains(x))
        columnBuffer = columnBuffer :+ Laser
      else
        columnBuffer = columnBuffer :+ Empty
    }

    var frameBuffer: List[List[CellType]] = List[List[CellType]]()
    for (i <- 0 until gridDims.width - 10) {
      frameBuffer = frameBuffer :+ columnBuffer
    }

    frameBuffer = laser_right +: frameBuffer
    frameBuffer = frameBuffer :+ laser_left

    for (i <- 0 until 6) {
      frameBuffer = List.fill(gridDims.height)(Empty) +: frameBuffer
      frameBuffer = frameBuffer :+ List.fill(gridDims.height)(Empty)
    }

    // first frame with sound
    for (x <- 0 until 5) res.enqueue(Frame(frameBuffer, List[String]()))

    // post fire
    for (x <- 0 until 30) res.enqueue(Frame(weaponWarmedUp, List[String]()))

    res

  }

}

