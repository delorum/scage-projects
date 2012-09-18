package su.msk.dunno.scage.tutorials.gravitation

import net.scage.support.{State, Vec}
import net.scage.support.tracer3.{DefaultTrace, Trace}
import Orbita._

class MaterialPoint(init_velocity:Vec, init_acceleration:Vec, val mass:Float, val radius:Int) extends Trace {
  protected var _velocity = init_velocity
  def velocity = _velocity
  def updateVelocity(other_bodies:Seq[MaterialPoint]) {
    _velocity += (gravitationAcceleration(other_bodies) + _acceleration)*dt
  }

  protected var _acceleration = init_acceleration
  def acceleration = _acceleration
  def applyForce(force:Vec) {
    _acceleration = _acceleration + force/mass
  }
  def removeAcceleration() {_acceleration = Vec(0,0)}

  def state = new State()
  def changeState(changer:Trace, s:State) {}

  // gravitation force
  def gravitationAcceleration(other_bodies:Seq[MaterialPoint]) = {
    other_bodies.filter(other_body =>
      id != other_body.id &&
      location.dist(other_body.location) > radius+other_body.radius).foldLeft(Vec(0,0))(
      (gravitational_acceleration, other_body) => {
        val vec = other_body.location - location
        val distance = vec.norma
        gravitational_acceleration + vec*G*other_body.mass/(distance*distance*distance)
    })
  }

  def copy = {
    val this_id = id
    new MaterialPoint(velocity, acceleration, mass, radius) {
      override val id = this_id
    }
  }
}