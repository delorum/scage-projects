package net.scageprojects.drivecontrol

import net.scage.ScageLib._
import com.weiglewilczek.slf4s.Logger

object DriveControl extends ScageScreenApp("Drive Control", 800, 600) {
  val tracer = CoordTracer(solid_edges = false)
  val car = new Car(windowCenter)

  key(KEY_A, onKeyDown = car.switchLeftLights())
  key(KEY_S, onKeyDown = car.switchHazardLights())
  key(KEY_D, onKeyDown = car.switchRightLights())

  key(KEY_LEFT, 10, onKeyDown = car.turnLeft())
  key(KEY_RIGHT, 10, onKeyDown = car.turnRight())
  key(KEY_UP, onKeyDown = car.gasEnabled(), onKeyUp = car.gasDisabled())
}

import DriveControl._

object Car {
  val LIGHTS_OFF = 0
  val LEFT_LIGHTS = 1
  val RIGHT_LIGHTS = 2
  val HAZARD_LIGHTS = 3

  val light_modes = List(LIGHTS_OFF, LEFT_LIGHTS, RIGHT_LIGHTS, HAZARD_LIGHTS)
}

import Car._

class Car(start_position:Vec) extends DefaultTrace {
  private val log = Logger(this.getClass.getName)
  private var lights_mode = LIGHTS_OFF
  private var left_light_on = false
  private var right_light_on = false

  def lights = lights_mode
  def lights_=(new_lights_mode:Int) {
    if(light_modes.contains(new_lights_mode)) {
      lights_mode = new_lights_mode
      startBlinking()
    }
  }

  def switchLeftLights() {
    lights_mode = lights_mode match {
      case LEFT_LIGHTS => LIGHTS_OFF
      case _ => LEFT_LIGHTS
    }
    startBlinking()
  }

  def switchRightLights() {
    lights_mode = lights_mode match {
      case RIGHT_LIGHTS => LIGHTS_OFF
      case _ => RIGHT_LIGHTS
    }
    startBlinking()
  }

  def switchHazardLights() {
    lights_mode = lights_mode match {
      case HAZARD_LIGHTS => LIGHTS_OFF
      case _ => HAZARD_LIGHTS
    }
    startBlinking()
  }

  private var is_blinking = false
  private def startBlinking() {
    if(!is_blinking) {
      is_blinking = true
      var _period = 1000
      def period = {(if(_period == 1000) _period = 500 else _period = 1000); _period}
      actionDynamicPeriod(period) {
        lights_mode match {
          case LEFT_LIGHTS =>
            if(right_light_on) right_light_on = false
            left_light_on = !left_light_on
          case RIGHT_LIGHTS =>
            if(left_light_on) left_light_on = false
            right_light_on = !right_light_on
          case HAZARD_LIGHTS =>
            if(left_light_on != right_light_on) {
              left_light_on = right_light_on
            }
            left_light_on = !left_light_on
            right_light_on = !right_light_on
          case _ =>
            left_light_on = false
            right_light_on = false
            is_blinking = false
            deleteSelf()
        }
      }
    }
  }

  private var wheel_rotation = 0.0f
  private val wheel_rotation_speed = 0.5f
  private var car_rotation = 0.0f
  private val car_rotation_speed = 1.0f

  def turnLeft() {
    if(wheel_rotation < 35) wheel_rotation += wheel_rotation_speed
  }
  def turnRight() {
    if(wheel_rotation > -35) wheel_rotation -= wheel_rotation_speed
  }

  private var speed       = 0.0f
  private val k           = 0.05f
  private val motor_force = 0.1f
  private var gas_enabled = false
  private val mass        = 1f
  private val dt          = 1f

  action(50) {
    speed += ((if(gas_enabled) motor_force else 0f) - (if(speed > 0) k else 0f))/mass*dt
    if(speed.abs < 0.1f) speed = 0f
    val new_location = location + Vec(0,1).rotateDeg(car_rotation)*speed*dt
    if(speed > 0) {
      if(wheel_rotation != 0) {
        car_rotation += wheel_rotation.signum*car_rotation_speed
        wheel_rotation -= wheel_rotation.signum*car_rotation_speed
      }
    }
    log.info("speed: "+speed)
    tracer.updateLocation(this, new_location)
  }

  def gasEnabled() {
    gas_enabled = true
  }
  def gasDisabled() {
    gas_enabled = false
  }

  tracer.addTrace(start_position, this)
  render {
    openglMove(location)
    openglRotateDeg(car_rotation)
    drawRectCentered(Vec.zero, 20, 50, WHITE)
    if(left_light_on) {
      drawFilledRectCentered(Vec.zero + Vec(-10, 25), 5, 5, ORANGE)
      drawFilledRectCentered(Vec.zero + Vec(-10, -25), 5, 5, ORANGE)
    }
    if(right_light_on) {
      drawFilledRectCentered(Vec.zero + Vec(10, 25), 5, 5, ORANGE)
      drawFilledRectCentered(Vec.zero + Vec(10, -25), 5, 5, ORANGE)
    }

    openglMove(Vec(0, 20))
    openglRotateDeg(wheel_rotation)
    drawLine(Vec(-15, 0), Vec(15, 0), WHITE)
  }
}
