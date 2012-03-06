package net.scage.tutorial.jetflight

import net.scage.ScageLib._
import net.scage.support.tracer3.{Trace, CoordTracer}
import net.scage.{Scage, ScageScreenApp}
import net.scage.support.{ScageColor, State, Vec}
import net.scage.tutorial.netflight.{FlyingObject, Plane}

object JetFlight extends ScageScreenApp(unit_name = "Jet Flight") {
  val EXPLOSION_ANIMATION = animation("explosion_animation.png", 36, 35, 72, 69, 3)
  val LAND                = image("land.png", 800, 600, 0, 0, 800, 600)

  val tracer = CoordTracer(solid_edges = false)

  OurPlane
  EnemyPlane

  backgroundColor = WHITE
  render(-10) {
    drawDisplayList(LAND, windowCenter)
  }
  
  interface {
    if(onPause) {
      if(OurPlane.health <= 0) print("You lose. Play Again? (Y/N)", windowCenter, RED)
      else if(EnemyPlane.health <= 0) print("You win! Play Again? (Y/N)", windowCenter, RED)
    }
  }

  keyNoPause(KEY_Y, onKeyDown = if(onPause) {restart(); pauseOff()})
  keyNoPause(KEY_N, onKeyDown = if(onPause) Scage.stopApp())
}

trait FlyingObject {
  protected var speed = 5.0f
  protected var rotation = 0.0f

  def step = Vec(-0.4f*speed*math.sin(math.toRadians(rotation)).toFloat,
                 0.4f*speed*math.cos(math.toRadians(rotation)).toFloat)
}

import JetFlight._

trait Plane extends FlyingObject {
  protected var _health = 100
  def health = _health

  protected var plane_side = 1
}

object OurPlane extends Plane {
  init {
    _health = 100
    tracer.updateLocation(trace, windowCenter)
    rotation = 0.0f
  }

  val trace = tracer.addTrace(windowCenter, new Trace {
    def state = State("type" -> "plane", "health" -> _health)
    def changeState(changer:Trace, s:State) {
      s.neededKeys {
        case ("damage", damage:Int) => _health -= damage
      }
    }
  })

  key(KEY_LEFT,  10, onKeyDown = rotation += 0.2f*speed)
  key(KEY_RIGHT, 10, onKeyDown = rotation -= 0.2f*speed)
  key(KEY_UP,    10, onKeyDown = if(speed < 15) speed += 0.5f)

  key(KEY_LCONTROL, onKeyDown = {
    new Rocket(trace.id, tracer.outsideCoord(trace.location + step.n.rotate(math.Pi/2 * plane_side)*10), speed, rotation)
    plane_side *= -1
  })

  action {
    if(_health <= 0) pause()

    tracer.updateLocation(trace, trace.location + step)
    if(speed > 5) speed -= 0.1f
  }

  val PLAYER_PLANE = image("plane.png", 60, 60, 0, 0, 122, 121)
  render {
    openglMove(trace.location)
    openglRotate(rotation)
    drawDisplayList(PLAYER_PLANE, Vec.zero)
  }

  interface {
    drawFilledRect(Vec(20, window_height-20), window_width/2*health/100, 10, RED)
  }
}

object EnemyPlane extends Plane {
  init {
    _health = 100
    tracer.updateLocation(trace, tracer.randomCoord())
    rotation = (math.random*20-10).toFloat
  }

  rotation = (math.random*20-10).toFloat

  val trace = tracer.addTrace(tracer.randomCoord(), new Trace {
    def state = State("type" -> "plane", "health" -> _health)
    def changeState(changer:Trace, s:State) {
      s.neededKeys {
        case ("damage", damage:Int) => _health -= damage
      }
    }
  })

  private var shoot_cooldown = 0
  action {
    if(_health <= 0) pause()

    tracer.updateLocation(trace, trace.location + step)

    val enemy_planes = tracer.tracesNearCoord(trace.location, -8 to 8,
      condition = other_trace => {
        other_trace.id != trace.id &&
        other_trace.state.valueOrDefault("type", "unknown type") == "plane" &&
        other_trace.state.valueOrDefault("health", 0) > 0
      }
    )
    if(!enemy_planes.isEmpty) {
      val plane = enemy_planes.head
      val planes_angle = (plane.location - trace.location) rad step
      val planes_side = math.signum((plane.location - trace.location) * step)
      if(planes_angle < math.Pi/12) {
        if(shoot_cooldown <= 0) {
          new Rocket(trace.id, tracer.outsideCoord(trace.location + step.n.rotate(math.Pi/2 * plane_side)*10), speed, rotation);
          shoot_cooldown = 10
          plane_side *= -1
        } else shoot_cooldown -= 1
      } else rotation += 0.2f*speed*planes_side
    }
  }
  
  val ENEMY_PLANE = image("plane2.png", 60, 60, 0, 0, 122, 121)
  render {
    openglMove(trace.location)
    openglRotate(rotation)
    drawDisplayList(ENEMY_PLANE, Vec.zero)
  }

  interface {
    drawFilledRect(Vec(window_width-20, window_height-20), -window_width/2*health/100, 10, YELLOW)
  }
}

class Rocket(shooter_id:Int, init_coord:Vec, init_speed:Float, init_rotation:Float) extends FlyingObject {
  private var fuel = 60
  rotation = init_rotation
  speed = init_speed + 10

  val trace = tracer.addTrace(init_coord, new Trace {
    def state = State("type" -> "rocket")
    def changeState(changer:Trace, s:State) {}
  })

  action {
    if(fuel > 0) {
      tracer.updateLocation(trace, trace.location + step)

      val target_planes = tracer.tracesNearCoord(trace.location, -1 to 1,
        condition = other_trace =>
          other_trace.id != trace.id &&
          other_trace.id != shooter_id &&
          other_trace.state.valueOrDefault("type", "unknown type") == "plane" &&
          other_trace.state.valueOrDefault("health", 0) > 0 &&
          (other_trace.location dist trace.location) < (10+30))
      if(!target_planes.isEmpty) {
        val damage = (math.random*20).toInt
        target_planes.foreach(_.changeState(trace, State("damage" -> damage)))
        lazy val flying_word_color = shooter_id match {
          case OurPlane.trace.id => RED
          case EnemyPlane.trace.id => YELLOW
          case _ => RED
        }
        new FlyingWord(damage, flying_word_color, trace.location, step)
        fuel = 0
      }

      fuel -= 1
      next_frame += 1
      if(next_frame >= ROCKET_ANIMATION.length) next_frame = 0
    } else {
      deleteSelf()
      tracer.removeTraces(trace)
    }
  }

  val ROCKET_ANIMATION = animation("rocket_animation.png", 10, 29, 14, 44, 3)
  private var next_frame = 0
  render {
    if(fuel > 0) {
      openglMove(trace.location)
      openglRotate(rotation)
      drawDisplayList(ROCKET_ANIMATION(next_frame), Vec.zero)
    } else {
      openglMove(trace.location)
      drawDisplayList(EXPLOSION_ANIMATION(0), Vec.zero)
      deleteSelf()
    }
  }
}

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) extends FlyingObject {
  private var lifetime = 60;
  private val dir = direction.n
  private var coord = init_coord

  action {
    if(lifetime > 0) {
      coord += dir
      lifetime -= 1
    } else deleteSelf()
  }

  render {
    if(lifetime > 0) {
      print(message, coord, color)
    } else deleteSelf()
  }
}
