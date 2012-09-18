package su.msk.dunno.scage.tutorials.gravitation

import net.scage.support.Vec
import org.lwjgl.opengl.GL11
import net.scage.ScageLib._
import Orbita._

class SpaceShip(init_velocity:Vec)
extends MaterialPoint(init_velocity, Vec(0,0), property("ship.mass", 0.1f), 1) {
  protected var _rotation = 0.0f
  def rotation = _rotation

  protected var _direction = Vec(0, 1)
  def direction = _direction

  def rotate(deg:Float) {
    _direction = _direction.rotateDeg(deg)
    _rotation += deg
  }

  protected var acceleration_period = 0.0f
  def accelerationPeriod = acceleration_period
  protected var current_acceleration_moment = 0.0f
  def currentAccelerationMoment = current_acceleration_moment
  def increaseAccelerationPeriod(multiplier:Int = 1) {
    acceleration_period += dt*multiplier
    current_acceleration_moment = acceleration_period
  }
  def decreaseAccelerationPeriod(multiplier:Int = 1) {
    if(acceleration_period > dt*multiplier) {
      acceleration_period -= dt*multiplier
      current_acceleration_moment = acceleration_period
    }
  }
  def setAccelerationMoment(new_moment:Float) {
    current_acceleration_moment = new_moment
  }

  override def updateVelocity(other_bodies:Seq[MaterialPoint]) {
    _velocity += (gravitationAcceleration(other_bodies) + _acceleration)*dt
    if(current_acceleration_moment > 0) {
      current_acceleration_moment -= dt
      if(current_acceleration_moment <= 0) {
        _acceleration = Vec(0,0)
        current_acceleration_moment = 0
      }
    }
  }

  override def copy = {
    val this_id = id
    if(acceleration_period > 0) {
      new MaterialPoint(velocity, acceleration, mass, radius) {
        override val id = this_id
        protected var period = acceleration_period
        override def updateVelocity(other_bodies:Seq[MaterialPoint]) {
          _velocity += (gravitationAcceleration(other_bodies) + _acceleration)*dt
          if(period > 0) {
            period -= dt
            if(period <= 0) {
              _acceleration = Vec(0,0)
              period = 0
            }
          }
        }
      }
    }
    else new MaterialPoint(velocity, acceleration, mass, radius) {
      override val id = this_id
    }
  }

  interface {
    print("velocity: "+_velocity.norma, 10, windowHeight-20, YELLOW)
    print("acceleration: "+_acceleration, 10, windowHeight-40, YELLOW)
    print("period: "+acceleration_period, 10, windowHeight-60, YELLOW)
    print("moment: "+current_acceleration_moment, 10, windowHeight-80, YELLOW)
  }

  //private val SHIP = image("plane2.png", 3, 3, 0, 0, 122, 121)
  render {
    currentColor = GREEN
    GL11.glPushMatrix()
      GL11.glTranslatef(location.x, location.y, 0.0f)
      GL11.glRotatef(_rotation, 0.0f, 0.0f, 1.0f)
      //GL11.glCallList(SHIP)
      drawFilledPolygon(Vec(-1,-1), Vec(1,-1), Vec(0,2))
    GL11.glPopMatrix()

    /*GL11.glPushMatrix();
      GL11.glTranslatef(coord.x, coord.y, 0.0f)
      GL11.glScalef(1.5f/scale, 1.5f/scale, 1)
      print(mass, 0, 0, YELLOW)
      print(coord, 0, -15, YELLOW)
      print(_velocity, 0, -30, YELLOW)
    GL11.glPopMatrix()*/

    drawLine(location, location+_direction*20, RED)
    drawLine(location, location+_velocity, CYAN)
  }
}