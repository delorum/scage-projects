package ${package}

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scage.ScageScreenApp
import com.github.dunnololda.scage.support.tracer3.{CoordTracer, DefaultTrace}
import com.github.dunnololda.scage.support.{ScageColor, Vec}

import scala.collection.mutable.ArrayBuffer

object LightCyclesOffline extends ScageScreenApp("Light Cycles", 640, 480) {
  val tracer = CoordTracer.create[LightCycleTrace](
    field_from_x = 10,
    field_to_x   = 610,
    field_from_y = 10,
    field_to_y   = 460,
    solid_edges = true
  )

  def randomDir = {
    (math.random*4).toInt match {
      case 0 => Vec( 0, 1)
      case 1 => Vec( 0,-1)
      case 2 => Vec( 1, 0)
      case 3 => Vec(-1, 0)
    }
  }

  val borders = List(Vec(tracer.field_from_x, tracer.field_from_y),
    Vec(tracer.field_from_x, tracer.field_to_y-1),
    Vec(tracer.field_to_x-1, tracer.field_to_y-1),
    Vec(tracer.field_to_x-1, tracer.field_from_y),
    Vec(tracer.field_from_x, tracer.field_from_y)).sliding(2).map {
    case List(a, b) => (a, b)
  }.toList
  def otherLines(cycle_id:Int):List[(Vec, Vec)] = {
    val other_cycles = tracer.tracesList.filter(oc => oc.id != cycle_id)
    val our_cycle = tracer.tracesList.filter(oc => oc.id == cycle_id && oc.prevLocations.length > 2)
    borders ++
      other_cycles.flatMap {
        c => (c.prevLocations.toList ++ List(c.location)).sliding(2).map {
          case List(a, b) => (a, b)
        }
      } ++
      our_cycle.flatMap {
        c => c.prevLocations.init.toList.sliding(2).map {
          case List(a, b) => (a, b)
        }
      }
  }

  def interpoint(v1:Vec, v2:Vec, v3:Vec, v4:Vec):Vec = {
    val Vec(a1x, a1y) = v1
    val Vec(a2x, a2y) = v2
    val Vec(b1x, b1y) = v3
    val Vec(b2x, b2y) = v4

    val a1 = a1x - a2x
    val b1 = b2x - b1x
    val c1 = a1x - b1x

    val a2 = a1y - a2y
    val b2 = b2y - b1y
    val c2 = a1y - b1y

    val d  = a1*b2 - a2*b1
    val dx = c1*b2 - c2*b1
    //val dy = a1*c2 - a2*c1

    val ta = dx/d
    //val tb = dy/d

    val x = a1x + ta*(a2x - a1x)
    val y = a1y + ta*(a2y - a1y)

    Vec(x, y)
  }

  def checkCrash(cycle:LightCycleTrace):Boolean = {
    otherLines(cycle.id).find {
      case (a1, a2) => areLinesIntersect(a1, a2, cycle.location, cycle.prevLocations.last)
    } match {
      case Some((a1, a2)) =>
        val i = interpoint(a1, a2, cycle.location, cycle.prevLocations.last)
        i.dist2(cycle.location) < 1
      case None => false
    }
  }

  UserCycleOffline
  Program1CycleOffline
  Program2CycleOffline
  Program3CycleOffline

  private var user_count  = 0
  private var program1_count  = 0
  private var program2_count  = 0
  private var program3_count  = 0

  private var result       = -1   // 0 - player won, 1 - enemy1 won, 2 - enemy2 won, 3 - enemy3 won, 4 - nobody won
  init {
    result = -1
  }
  action(10) {
    tracer.tracesList.length match {
      case 0 =>
        result = 4
        pause()
      case 1 =>
        tracer.tracesList.head match {
          case UserCycleOffline =>
            user_count += 1
            result = 0
            pause()
          case Program1CycleOffline =>
            program1_count += 1
            result = 1
            pause()
          case Program2CycleOffline =>
            program2_count += 1
            result = 2
            pause()
          case Program3CycleOffline =>
            program3_count += 1
            result = 3
            pause()
        }
      case _ =>
        tracer.tracesList.foreach(cycle => {
          if(checkCrash(cycle)) cycle.crash()
        })
    }
  }

  keyIgnorePause(KEY_F2, onKeyDown = restart())
  keyIgnorePause(KEY_SPACE, onKeyDown = {
    result match {
      case 0 | 1 | 2 | 3 | 4 =>
        pauseOff()
        restart()
      case _ =>
        switchPause()
    }
  })

  render(-10) {
    print(user_count+"\n\n"+program1_count+"\n\n"+program2_count+"\n\n"+program3_count, windowWidth-15, windowHeight-160, DARK_GRAY, align = "center-left")
    drawTraceGrid(tracer, DARK_GRAY)
    if(onPause) {
      result match {
        case 0 => print("USER WON. PRESS SPACE",     windowCenter, RED,       align = "center")
        case 1 => print("PROGRAM1 WON. PRESS SPACE", windowCenter, YELLOW,    align = "center")
        case 2 => print("PROGRAM2 WON. PRESS SPACE", windowCenter, BLUE,      align = "center")
        case 3 => print("PROGRAM3 WON. PRESS SPACE", windowCenter, WHITE,     align = "center")
        case 4 => print("NOBODY WON. PRESS SPACE",   windowCenter, DARK_GRAY, align = "center")
        case _ => print("PAUSE. PRESS SPACE",        windowCenter, DARK_GRAY, align = "center")
      }
    }
  }
}

import LightCyclesOffline._

abstract class LightCycleTrace(val color:ScageColor) extends DefaultTrace {
  protected val prev_locations = ArrayBuffer[Vec]()
  def prevLocations:Seq[Vec] = prev_locations

  private var _dir:Vec = randomDir
  def dir = _dir
  protected def dir_=(new_dir:Vec) {
    prev_locations += location
    _dir = new_dir
  }

  protected var is_crashed = false
  def isCrashed = is_crashed
  def crash()
}

object UserCycleOffline extends LightCycleTrace(RED) {
  init {
    is_crashed = false
    tracer.addTrace(tracer.randomCoord(), this)
    dir = randomDir
  }

  action(10) {
    if(!is_crashed) tracer.updateLocation(id, location + dir)
  }

  key(KEY_W, onKeyDown = if(!is_crashed && dir != Vec( 0, -1)) dir = Vec( 0, 1))
  key(KEY_A, onKeyDown = if(!is_crashed && dir != Vec( 1,  0)) dir = Vec(-1, 0))
  key(KEY_S, onKeyDown = if(!is_crashed && dir != Vec( 0,  1)) dir = Vec( 0,-1))
  key(KEY_D, onKeyDown = if(!is_crashed && dir != Vec(-1,  0)) dir = Vec( 1, 0))

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  render {
    if(!is_crashed) {
      drawSlidingLines(prev_locations, color)
      drawLine(prev_locations.last, location, color)
    }
  }

  clear {
    crash()
  }

  def crash() {
    if(!is_crashed) {
      tracer.removeTraceById(id)
      prev_locations.clear()
      is_crashed = true
    }
  }
}

abstract class EnemyCycle(val max_warning_distance:Int,
                          val turn_probability:Double,
                          val check_distance:Int,
                          val aggression_factor:Double,
                          color:ScageColor) extends LightCycleTrace(color) {
  this:LightCycleTrace =>

  protected def minObstacleDist(d:Vec):Float = {
    otherLines(id).filter {
      case (a1, a2) => areLinesIntersect(a1, a2, location, location + d*640)
    }.map {
      case (a1, a2) => interpoint(a1, a2, location, location + d*640)
    }.foldLeft(1000f) {
      case (min_dist, i) =>
        val dist = i.dist(location)
        if(dist < min_dist) dist else min_dist
    }
  }

  private var warning_distance = max_warning_distance
  protected def ai() {
    if(otherLines(id).exists {
      case (a1, a2) => areLinesIntersect(a1, a2, location, location + dir * warning_distance)
    } || math.random < turn_probability) {
      dir = selectTurn(check_distance, aggression_factor)
      warning_distance -= (1 + math.random*4).toInt     // maybe make 4 a parameter
      if(warning_distance < 1) warning_distance = max_warning_distance
    }
  }

  protected def selectTurn(check_distance:Int, aggression_factor:Double) = {
    dir match {
      case Vec(0,1) | Vec( 0,-1) =>
        (otherLines(id).exists {
          case (a1, a2) => areLinesIntersect(a1, a2, location, location + Vec(1,0)*check_distance)
        },
          otherLines(id).exists {
            case (a1, a2) => areLinesIntersect(a1, a2, location, location + Vec(-1,0)*check_distance)
          }) match {
          case (true,  false) => Vec(-1,0)
          case (false, true)  => Vec( 1,0)
          case (true,  true)  =>
            val (min_right, min_left) = (minObstacleDist(Vec(1,0)), minObstacleDist(Vec(-1,0)))
            if(min_right > min_left) Vec(1,0) else Vec(-1,0)
          case (false, false) =>
            if(!UserCycleOffline.isCrashed && math.random < aggression_factor) {   // turn to the player
            val player_side = math.signum((UserCycleOffline.location - location).x)
              if(player_side != 0) Vec(player_side, 0)
              else if(math.random > 0.5) Vec(1,0) else Vec(-1,  0)
            } else if(math.random > 0.5) Vec(1,0) else Vec(-1,  0)
        }
      case Vec(1,0) | Vec(-1, 0) =>
        (otherLines(id).exists {
          case (a1, a2) => areLinesIntersect(a1, a2, location, location + Vec(0,1)*check_distance)
        },
          otherLines(id).exists {
            case (a1, a2) => areLinesIntersect(a1, a2, location, location + Vec(0,-1)*check_distance)
          }) match {
          case (true,  false) => Vec( 0,-1)
          case (false, true)  => Vec( 0, 1)
          case (true,  true)  =>
            val (min_up, min_down) = (minObstacleDist(Vec(0,1)), minObstacleDist(Vec(0,-1)))
            if(min_up > min_down) Vec(0,1) else Vec(0,-1)
          case (false, false) =>
            if(math.random < aggression_factor) {   // turn to the player
            val player_side = math.signum((UserCycleOffline.location - location).y)
              if(player_side != 0) Vec(0, player_side)
              else if(math.random > 0.5) Vec(0,1) else Vec(0,-1)
            } else if(math.random > 0.5) Vec(0,1) else Vec(0,-1)
        }
      case _ => dir
    }
  }
}

object Program1CycleOffline extends EnemyCycle(
  max_warning_distance = 20,
  turn_probability     = 0.005,
  check_distance       = 50,
  aggression_factor    = 0.7,
  color = YELLOW) {
  init {
    is_crashed = false
    tracer.addTrace(tracer.randomCoord(), this)
    dir = randomDir
  }

  // simple ai:
  // 1. trace a little further and check for obstacles
  // 2. make turns at random monets
  action(10) {
    if(!is_crashed) ai()
  }

  action(10) {
    if(!is_crashed) tracer.updateLocation(id, location + dir)
  }

  render {
    if(!is_crashed) {
      drawSlidingLines(prev_locations, color)
      drawLine(prev_locations.last, location, color)
    }
  }

  clear {
    crash()
  }

  def crash() {
    if(!is_crashed) {
      tracer.removeTraceById(id)
      prev_locations.clear()
      is_crashed = true
    }
  }
}

object Program2CycleOffline extends EnemyCycle(
  max_warning_distance = 30,
  turn_probability     = 0.005,
  check_distance       = 70,
  aggression_factor    = 0.6,
  color = CYAN) {
  init {
    is_crashed = false
    tracer.addTrace(tracer.randomCoord(), this)
    dir = randomDir
  }

  // simple ai:
  // 1. trace a little further and check for obstacles
  // 2. make turns at random moments
  action(10) {
    if(!is_crashed) ai()
  }

  action(10) {
    if(!is_crashed) tracer.updateLocation(id, location + dir)
  }

  render {
    if(!is_crashed) {
      drawSlidingLines(prev_locations, color)
      drawLine(prev_locations.last, location, color)
    }
  }

  clear {
    crash()
  }

  def crash() {
    if(!is_crashed) {
      tracer.removeTraceById(id)
      prev_locations.clear()
      is_crashed = true
    }
  }
}

object Program3CycleOffline extends EnemyCycle(
  max_warning_distance = 40,
  turn_probability     = 0.005,
  check_distance       = 90,
  aggression_factor    = 0.5,
  color = WHITE) {
  init {
    is_crashed = false
    tracer.addTrace(tracer.randomCoord(), this)
    dir = randomDir
  }

  // simple ai:
  // 1. trace a little further and check for obstacles
  // 2. make turns at random monets
  action(10) {
    if(!is_crashed) ai()
  }

  action(10) {
    if(!is_crashed) tracer.updateLocation(id, location + dir)
  }

  render {
    if(!is_crashed) {
      drawSlidingLines(prev_locations, color)
      drawLine(prev_locations.last, location, color)
    }
  }

  clear {
    crash()
  }

  def crash() {
    if(!is_crashed) {
      tracer.removeTraceById(id)
      prev_locations.clear()
      is_crashed = true
    }
  }
}
