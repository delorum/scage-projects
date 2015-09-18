package com.github.dunnololda.scageprojects.uke

import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.ScageLib._
import org.newdawn.slick.openal.{AudioLoader, SoundStore}
import org.newdawn.slick.util.ResourceLoader

import scala.collection.mutable.ArrayBuffer

object World extends ScageScreenApp("Uke", 800, 600) {
  private val log = MySimpleLogger(this.getClass.getName)

  private var uke_speed = 30
  def ukeSpeed = uke_speed
  def setDefaultSpeed() {
    uke_speed = 30
  }

  val physics = ScagePhysics(gravity = Vec(0, -10))
  physics.addPhysical(Uke)

  if(property("sound", false)) {
    val oggStream = AudioLoader.getStreamingAudio("OGG", ResourceLoader.getResource("resources/sounds/badapple.ogg"))
    init {
      oggStream.playAsMusic(1.0f, 1.0f, true)
      Unit
    }

    actionIgnorePause {
      SoundStore.get().poll(0)
    }
  }

  def loseCondition = {
    Uke.velocity.x < 10 || farthest_coord.y - Uke.coord.y > windowHeight
  }

  private var farthest_coord = Vec.zero
  private var lost = false
  init {
    lost = false
    farthest_coord = LevelBuilder.continueLevel(Vec(0, windowHeight/2-100), 0, 1000)

    action {
      if(loseCondition) {
        log.info("uke.velocity = " + Uke.velocity)
        lost = true
        pause()
        deleteSelf()
      }
    }
    Unit
  }

  action {
    physics.step()

    if(farthest_coord.x - Uke.coord.x < 1000) {
      log.info("adding new platforms..."+Uke.coord)
      farthest_coord = LevelBuilder.continueLevel(farthest_coord, 0, 1000)
    }
  }

  actionStaticPeriod(5000) {
    if(uke_speed < 50) {
      log.info("increasing speed:" + uke_speed)
      uke_speed += 3
    } else deleteSelf()
  }

  backgroundColor = WHITE
  center = Uke.coord + Vec(windowWidth/2-50, 0)

  keyIgnorePause(KEY_F2, onKeyDown = {
    restart()
    pauseOff()
  })

  keyIgnorePause(KEY_P, onKeyDown = if(!lost) switchPause())
  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp())

  interface {
    print("Z to jump (Z twice to double jump)\n" +
          "X to break obstacles\n" +
          "Down Arrow to fast down\n" +
          "P to pause current game\n" +
          "F2 to start the new one\n\n" +
          "Press P", 20, windowHeight-20)
    if(!onPause) {
      interface {
        print(Uke.coord.ix/100, 20, windowHeight-20)
        if(onPause) {
          if(lost) print("Oops! You smashed to death (press F2 to start new game)", 20, windowHeight-40)
          else print("Pause (Press P)", windowWidth/2, windowHeight/2)
        }
      }
      deleteSelf()
    }
  }

  pause()
}

import com.github.dunnololda.scageprojects.uke.World._

object Uke extends DynaBall(Vec(20, windowHeight/2-70), radius = 30, mass = 1.0f, true) {
  val uke_stand = image("uke-stand.png", 56, 70, 96, 0, 160, 200)         // 48, 60
  val uke_animation = animation("uke-animation.png", 56, 70, 160, 200, 6)

  private var frame = 0

  private val max_jump = 10
  private val max_jump2 = 10
  private val max_forward = 10
  private val forward_force = 3000

  private var num_jump    = 0
  private var num_jump2   = 0
  private var num_forward = 0

  def isForward = num_forward > (max_forward - 5)

  init {
    setDefaultSpeed()
    coord = Vec(20, windowHeight/2-70)
    velocity = Vec(ukeSpeed, 0)
  }

  key(KEY_Z, onKeyDown = {
    if(num_jump == 0 && num_jump2 == 0) {
      addForce(Vec(1000, 4000))
      num_jump = max_jump
    } else if(num_jump2 == 0) {
      velocity -= Vec(0, velocity.y)
      addForce(Vec(1000, 4000))
      num_jump2 = max_jump2
    }
  })

  key(KEY_X, onKeyDown = {
    if(num_forward == 0) {
      addForce(Vec(forward_force, 0))
      num_forward = max_forward
    }
  })

  key(KEY_DOWN, onKeyDown = addForce(Vec(0, -3000)))

  actionStaticPeriod(100) {
    frame += 1
    if(frame >= 6) frame = 0

    if(num_jump > 0)    num_jump -= 1
    if(num_jump2 > 0)   num_jump2 -= 1
    if(num_forward > 0) num_forward -= 1
  }

  action {
    val required_speed = ukeSpeed
    if(velocity.norma2 < required_speed*required_speed) velocity = velocity.n * required_speed
    else if(velocity.norma2 > (required_speed+5)*(required_speed+5)) velocity -= velocity.n
  }

  render {
    if(velocity.x != 0 && isTouching) {
      drawDisplayList(uke_animation(frame), coord)
    } else drawDisplayList(uke_stand, coord)
  }

  /*interface {
    print("velocity: "+velocity.norma, 20, windowHeight-20)
    print("velocity: "+velocity, 20, windowHeight-40)

    drawFilledRect(Vec(20, windowHeight-60), (100 - num_jump*10), 15, GRAY)
    drawFilledRect(Vec(20, windowHeight-80), (100 - num_jump2*10), 15, DARK_GRAY)
    drawFilledRect(Vec(20, windowHeight-100), (100 - num_forward*10), 15, BLUE)
  }*/
}

object LevelBuilder {
  //private val log = MySimpleLogger(this.getClass.getName)

  private val _platforms = ArrayBuffer[Physical]()
  def platforms = _platforms

  clear {
    while(_platforms.length > 0) removeOldestPlatform()
  }
  def addPlatform(platform:Physical) {
    physics.addPhysical(platform)
    _platforms += platform
    if(_platforms.length > 10) removeOldestPlatform()
  }
  def removeOldestPlatform() {
    val platform = _platforms.remove(0)
    physics.removePhysicals(platform)
  }

  def continueLevel(start:Vec, current_width:Int, required_width:Int):Vec = {
    if(current_width < required_width) {
      val random_width = (math.random*700).toInt + 2*ukeSpeed + 600
      val leftup_coord = if(start.x != 0) start + Vec((13*30) + (math.random*400).toInt, (100 + math.random*100).toInt)
                         else start

      val num_upper_points = 2 + (math.random*(7 + ukeSpeed/20)).toInt
      val platform_inner_points =
        (for(i <- 2 to num_upper_points)
        yield leftup_coord + Vec(i*random_width/num_upper_points, (-100 + math.random*100).toInt)).toList
      val farthest_coord = platform_inner_points.last
      val platform_points:List[Vec] =
        (List(leftup_coord, leftup_coord + Vec(random_width/num_upper_points, 0)) :::
        platform_inner_points :::
        List(leftup_coord+Vec(random_width, -windowHeight), leftup_coord+Vec(0, -windowHeight))).reverse
      val platform = new Platform(platform_points:_*)
      addPlatform(platform)

      if(platform_inner_points.length > 4 && math.random > 0.5) {
        val upper_platform = (math.random*2).toInt match {
          case 0 => infiniteUpperPlatform(platform_inner_points.init)
          case _ => upperPlatform(platform_inner_points.init.tail)
        }
        addPlatform(upper_platform)
      } else addRandomObstacle(leftup_coord, random_width, 10)

      continueLevel(farthest_coord, current_width + random_width + leftup_coord.ix - start.ix, required_width)
    } else start
  }

  def addRandomObstacle(start_coord:Vec, width:Int, height:Int) {
    if(math.random > 0.5) {
      val obstacle = new Obstacle(start_coord + Vec((math.random*width).toFloat, 10))
      addPlatform(obstacle)
    }
  }

  //TODO: add random obstacled to upper platforms

  def upperPlatform(points:List[Vec]) = {
    val upper_platform_points =
      ((for (point <- points) yield point + Vec(0, Uke.radius * 6)) :::
      (for (point <- points) yield point + Vec(0, Uke.radius * 5)).reverse).reverse
    new Platform(upper_platform_points:_*)
  }

  def infiniteUpperPlatform(points:List[Vec]) = {
    val upper_platform_points =
      (List(Vec(points.head.x, points.head.y + windowHeight), Vec(points.last.x, points.last.y + windowHeight)) :::
      (for (point <- points) yield point + Vec(0, Uke.radius * 5)).reverse).reverse
    new Platform(upper_platform_points:_*)
  }
}

class Platform(platform_points:Vec*) extends StaticPolygon(platform_points:_*) {
  val platform_color = randomColor
  render {
    if(physics.containsPhysical(this)) drawPolygon(points, /*platform_color*/BLACK)
    else {
      println("youo")
      deleteSelf()
    }
  }
}

class Obstacle(init_coord:Vec) extends DynaBox(init_coord, 70f, 70f, box_mass = 1000f, true) {
  val obstacle_color = randomColor
  render {
    if(physics.containsPhysical(this)) {
      drawPolygon(points, /*obstacle_color*/BLACK)
      //drawCircle(coord, 100, obstacle_color)
    } else deleteSelf()
  }

  action {
    if(coord.dist(Uke.coord) < 100 && Uke.isForward) {
      physics.removePhysicals(this)
      deleteSelf()
    }
  }
}