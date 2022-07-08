// DO NOT MODIFY FOR BASIC SUBMISSION
// scalastyle:off

package undertale.game

import java.awt.event
import java.awt.event.KeyEvent._

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import processing.core.{PApplet, PConstants, PImage}
import processing.event.KeyEvent
import undertale.logic._
import undertale.game.UndertaleGame._
import undertale.logic.{Point => GridPoint}

import ddf.minim.Minim

class UndertaleGame extends GameBase {

  var gameLogic: UndertaleLogic = UndertaleLogic()
  val updateTimer = new UpdateTimer(UndertaleLogic.FramesPerSecond.toFloat)
  val gridDims: Dimensions = gameLogic.gridDims
  val widthInPixels: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
  val heightInPixels: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
  val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)

  // audio effects
  val minim = new Minim(this)

  // ggFlag
  var ggInitialed: Boolean = false

  // multikey press
  var w_pressed: Boolean = false
  var s_pressed: Boolean = false
  var a_pressed: Boolean = false
  var d_pressed: Boolean = false

  override def draw(): Unit = {
    if (gameLogic.isGameOver) {
      drawGameOverScreen()

    }

    else if (gameLogic.damageFlash) {
      minim.loadSample("src/resources/take_dmg.mp3").trigger()
      background(255, 0, 0)
      gameLogic.damageFlash = false
    }

    else {
      if (gameLogic.positionModified) {
        minim.loadSample("src/resources/ping.mp3").trigger()
        gameLogic.positionModified = false
      }
      background(0)
      drawTextCentered(gameLogic.onPlayer.health.toString, 50, screenArea.centerDown)
      updateState()
      drawGrid()
    }
  }

  def drawGameOverScreen(): Unit = {
    if (!ggInitialed) {
      minim.stop()
      // switch music
      minim.loadSample("src/resources/heartbreak.mp3").trigger()
      Thread.sleep(4000)
      minim.loadFile("src/resources/determination.mp3").loop()
      ggInitialed = true
    }

    background(0, 0, 0)
    val gameoverImage: PImage = loadImage("src/resources/gameover.png")
    image(gameoverImage, screenArea.centerX, screenArea.centerY)
  }

  def drawGrid(): Unit = {

    val widthPerCell = screenArea.width / gridDims.width
    val heightPerCell = screenArea.height / gridDims.height

    // cell images
    val soul: PImage = loadImage("src/resources/soul.png")

    val bone_vert_top: PImage = loadImage("src/resources/bone_vert_top.png")
    val bone_vert_bot: PImage = loadImage("src/resources/bone_vert_bot.png")
    val bone_vert_mid: PImage = loadImage("src/resources/bone_vert_mid.png")

    val bone_hori_top: PImage = loadImage("src/resources/bone_hori_left.png")
    val bone_hori_bot: PImage = loadImage("src/resources/bone_hori_right.png")
    val bone_hori_mid: PImage = loadImage("src/resources/bone_hori_mid.png")

    val laserHead_up: PImage = loadImage("src/resources/laserhead_up.png")
    val laserHead_down: PImage = loadImage("src/resources/laserhead_down.png")
    val laserHead_left: PImage = loadImage("src/resources/laserhead_left.png")
    val laserHead_right: PImage = loadImage("src/resources/laserhead_right.png")

    val platform: PImage = loadImage("src/resources/platform.png")

    // logic

    for (p <- gridDims.allPointsInside) {
      drawCell(getCell(p), gameLogic.getCellType(p))
    }

    def getCell(p: GridPoint): Rectangle = {
      val leftUp = Point(screenArea.left + p.x * widthPerCell,
        screenArea.top + p.y * heightPerCell)
      Rectangle(leftUp, widthPerCell, heightPerCell)
    }

    def drawCell(area: Rectangle, cell: CellType): Unit = {
      // laser
      noStroke()

      cell match {
        case Soul => drawImage(soul, area)

        case BoneVerticalTop => drawImage(bone_vert_top, area)
        case BoneVerticalBot => drawImage(bone_vert_bot, area)
        case BoneVerticalMid => drawImage(bone_vert_mid, area)

        case BoneHorizontalLeft => drawImage(bone_hori_top, area)
        case BoneHorizontalRight => drawImage(bone_hori_bot, area)
        case BoneHorizontalMid => drawImage(bone_hori_mid, area)

        case LaserHeadLeft => drawImage(laserHead_left, area)
        case LaserHeadRight => drawImage(laserHead_right, area)
        case LaserHeadUp => drawImage(laserHead_up, area)
        case LaserHeadDown => drawImage(laserHead_down, area)
        case Laser => drawRectangle(area)

        case Platform => drawImage(soul, area)

        case Empty => ()
        case _ =>
      }
    }

  }

  /** Method that calls handlers for different key press events.
   * You may add extra functionality for other keys here.
   * See [[event.KeyEvent]] for all defined keycodes.
   *
   * @param event The key press event to handle
   */

  override def keyPressed(event: KeyEvent): Unit = {
    event.getKeyCode match {
      case VK_W => {
        if (d_pressed) gameLogic.moveUpRight()
        else if (a_pressed) gameLogic.moveUpLeft()
        else {
          w_pressed = true
          gameLogic.moveUp()
        }
      }
      case VK_S => {
        if (d_pressed) gameLogic.moveDownRight()
        else if (a_pressed) gameLogic.moveDownLeft()
        else {
          s_pressed = true
          gameLogic.moveDown()
        }
      }
      case VK_A => {
        if (w_pressed) gameLogic.moveUpLeft()
        else if (s_pressed) gameLogic.moveDownLeft()
        else {
          a_pressed = true
          gameLogic.moveLeft()
        }
      }
      case VK_D => {
        if (w_pressed) gameLogic.moveUpRight()
        else if (s_pressed) gameLogic.moveDownRight()
        else {
          d_pressed = true
          gameLogic.moveRight()
        }
      }

      // debug purpose
      case VK_H => {
        gameLogic.checkDmg()
      }

      case VK_C => {
        gameLogic.cheatMode()
      }

      case _ => ()
    }
  }

  override def keyReleased(event: KeyEvent): Unit = {
    event.getKeyCode match {
      case VK_W => w_pressed = false
      case VK_S => s_pressed = false
      case VK_A => a_pressed = false
      case VK_D => d_pressed = false
      case _ => ()
    }
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(widthInPixels, heightInPixels)
  }

  override def setup(): Unit = {
    // Fonts are loaded lazily, so when we call text()
    // for the first time, there is significant lag.
    // This prevents it from happening during gameplay.
    text("", 0, 0)

    // This should be called last, since the game
    // clock is officially ticking at this point
    updateTimer.init()

    imageMode(3)
    // background color
    background(0)

    // background music
    minim.loadSample("src/resources/battle_start.mp3").trigger()
    Thread.sleep(500)
    val backgroundMusic = minim.loadFile("src/resources/bgm.mp3")
    backgroundMusic.loop()

    // blink u cant see me
    def getTrashTalk(): String = {
      gameLogic.randomGen.randomInt(5) match {
        case 0 => "I almost feel bad for you, playing one game you can never win"
        case 1 => "No cheating, enjoying the boredom"
        case 2 => "you know you don't have to grade me THAT carefully right?"
        case 3 => "Made by Donghui"
        case 4 => "aka TK"
        case _ => "Ran out of smart talk"
      }
    }

    background(0, 0, 0)
    val sans: PImage = loadImage("src/resources/sans.png")
    image(sans, screenArea.centerX, screenArea.centerY)
    drawTextCentered(getTrashTalk(), 50, screenArea.centerDown)
    minim.loadSample("src/resources/sans.mp3").trigger()
  }

  def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      if (!gameLogic.isGameOver) {
        gameLogic.step()
      }
      // can not put this in draw, otherwise enjoy 60 lasers sound a frame hahaha
      playAudio()

      updateTimer.advanceFrame()
    }
  }

  def playAudio(): Unit = {
    if (gameLogic.frameQueue.nonEmpty) {
      val effects: List[String] = gameLogic.frameQueue.front.audio

      effects.foreach {
        case "laser" => minim.loadSample("src/resources/laser.mp3").trigger()
        case _ => ()
      }
    }
  }

}

object UndertaleGame {

  val WidthCellInPixels: Double = 15 * UndertaleLogic.DrawSizeFactor
  val HeightCellInPixels: Double = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    PApplet.main("undertale.game.UndertaleGame")
  }

}