package su.msk.dunno.scage.tutorials.gravitation

import net.scage.support.Vec
import net.scage.support.tracer3.DefaultTrace
import Orbita._
import collection.mutable.ArrayBuffer

class MaterialPoint(init_velocity:Vec     = Vec.zero,
                    init_acceleration:Vec = Vec.zero,
                    init_rotation:Float   = 0.0f,
                    init_direction:Vec    = Vec(0, 1),
                    val mass:Float,
                    val radius:Int) extends DefaultTrace {
  // Position, Time, Rotation, Direction, Acceleration, AccelerationPeriod
  protected val flight_plan:ArrayBuffer[(Vec, Long, Float, Vec, Vec, Int)] = ArrayBuffer()
  def addFlightPlan(new_flight_plan:Seq[(Vec, Long, Float, Vec, Vec, Int)]) {
    flight_plan ++= new_flight_plan
    println(flight_plan)
  }

  // Time, Rotation, Direction, Acceleration, AccelerationPeriod
  protected var current_event:Option[(Vec, Long, Float, Vec, Vec, Int)] = None
  protected var next_event:Option[(Vec, Long, Float, Vec, Vec, Int)]    = None

  protected var _velocity = init_velocity
  def velocity = _velocity
  def updateVelocity(time:Long, other_bodies:Seq[MaterialPoint]) {
    current_event match {
      case Some((event_position, event_time, event_rotation, event_direction, event_acceleration, event_period)) =>
        _acceleration = event_acceleration
        current_event = Some((event_position, event_time, event_rotation, event_direction, event_acceleration, event_period-1))
        if(event_period <= 0) {
          println(current_event+" ended")
          current_event = None
        }
      case None =>
        _acceleration = Vec.zero
    }
    next_event match {
      case Some((next_position, next_time, next_rotation, next_direction, next_event_acceleration, next_event_period)) =>
        if(time >= next_time) {
          current_event = next_event
          println("current_event: "+current_event)
          next_event    = flight_plan.find(_._2 > time)
          if(next_event == None) {
            println("no next event. pause")
          } else println("next_event: "+next_event)
        } else {
          val rotation_diff = next_rotation - rotation
          if(rotation_diff != 0) {
            val time_to_event = next_time - time
            val rotation_dt = rotation_diff/time_to_event
            //println("time: "+time+"/"+next_time+" rotation_diff: "+rotation_diff)
            rotate(rotation_dt)
          }
        }
      case None =>
        next_event = flight_plan.find(_._2 > time)
    }
    _velocity += (gravitationAcceleration(other_bodies) + _acceleration)*dt
  }

  protected var _rotation = init_rotation
  def rotation = _rotation

  protected var _direction = init_direction
  def direction = _direction

  def rotate(deg:Float) {
    _direction = _direction.rotateDeg(deg)
    _rotation += deg
  }

  protected var _acceleration = init_acceleration
  def acceleration = _acceleration
  def applyForce(force:Vec) {
    _acceleration += force/mass
  }
  def removeAcceleration() {_acceleration = Vec(0,0)}

  /*def state = new State()
  def changeState(changer:Trace, s:State) {}*/

  // gravitation force
  def gravitationAcceleration(other_bodies:Seq[MaterialPoint]) = {
    other_bodies.filter(other_body =>
      id != other_body.id &&
      location.dist(other_body.location) > radius+other_body.radius).foldLeft(Vec.zero)(
      (gravitational_acceleration, other_body) => {
        val vec = other_body.location - location
        val distance = vec.norma
        gravitational_acceleration + vec*G*other_body.mass/(distance*distance*distance)
    })
  }

  def copy = {
    val this_id = id
    new MaterialPoint(velocity, acceleration, rotation, direction, mass, radius) {
      override val id = this_id
    }
  }
}