package com.github.dunnololda.scageprojects.orbitalkiller.physics

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{Body => Phys2dBody, BodyList => Phys2dBodyList, Collider => Phys2dCollider, DynamicShape => Phys2dDynamicShape, Shape => Phys2dShape, StaticBody => Phys2dStaticBody}
import com.github.dunnololda.scageprojects.orbitalkiller.physics.Implicits._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.{MutableContact, PolygonShape, Shape}

import scala.collection.mutable.ArrayBuffer

class MutableBodyState(body: BodyState) {
  var active: Boolean = true

  val init_aabb = body.aabb
  val restitution = body.restitution

  val staticFriction = body.staticFriction
  val dynamicFriction = body.dynamicFriction
  val is_static = body.is_static
  val is_bullet = body.is_bullet
  val shape = body.shape
  val index = body.index
  val strIndex = index.toString

  private var _mass = body.mass

  def mass = _mass

  def mass_=(m: Double): Unit = {
    _mass = m
    _invMass = if (is_static || _mass == 0) 0.0 else 1.0 / _mass
    _I = _mass * shape.wI
    _invI = if (is_static || _I == 0) 0.0 else 1.0 / _I
  }

  private var _invMass = if (is_static || _mass == 0) 0.0 else 1.0 / _mass

  def invMass = _invMass

  private var _I = _mass * shape.wI

  def I = _I

  private var _invI = if (is_static || _I == 0) 0.0 else 1.0 / _I

  def invI = _invI

  // ===========================================

  var acc: DVec = DVec.zero

  var vel: DVec = body.vel
  /*def vel = _vel
  def vel_=(new_vel:DVec): Unit = {
    if(new_vel.x.abs > 300000000l || new_vel.y.abs > 300000000l) {
      throw new Exception("vel is set to infinity!")
    }
    _vel = new_vel
  }*/

  var coord: DVec = body.coord
  /*def coord = _coord
  def coord_=(new_coord:DVec): Unit = {
    if(new_coord.x.isNaN || new_coord.y.isNaN) {
      throw new Exception("coord is set to infinity!")
    }
    _coord = new_coord
  }*/

  var ang_acc: Double = 0.0
  var ang_vel: Double = body.ang_vel
  var ang: Double = body.ang

  var dacc: DVec = DVec.zero
  var dvel: DVec = DVec.zero
  var d_ang_acc: Double = 0.0
  var d_ang_vel: Double = 0.0

  val contacts = ArrayBuffer[MutableContact]()

  def init(): Unit = {
    acc = DVec.zero
    ang_acc = 0.0
    dacc = DVec.zero
    dvel = DVec.zero
    d_ang_acc = 0.0
    d_ang_vel = 0.0
    contacts.clear()
    aabb = shape.aabb(coord, ang)
  }

  var aabb = shape.aabb(coord, ang)

  def phys2dBody = {
    if (is_static) {
      val x = new Phys2dStaticBody(strIndex, shape.phys2dShape)
      x.setPosition(coord.x, coord.y)
      x.setRotation(ang.toRad)
      x.setUserData((index, shape))
      x
    } else {
      val x = new Phys2dBody(strIndex, shape.phys2dShape, mass)
      x.setPosition(coord.x, coord.y)
      x.setRotation(ang.toRad)
      x.adjustVelocity(vel.toPhys2dVecD)
      x.adjustAngularVelocity(ang_vel.toRad)
      x.setUserData((index, shape))
      x
    }
  }

  def phys2dBodyWithShape(shape: Shape) = {
    if (is_static) {
      val x = new Phys2dStaticBody(strIndex, shape.phys2dShape)
      x.setPosition(coord.x, coord.y)
      x.setRotation(ang.toRad)
      x.setUserData((index, shape))
      x
    } else {
      val x = new Phys2dBody(strIndex, shape.phys2dShape, mass)
      x.setPosition(coord.x, coord.y)
      x.setRotation(ang.toRad)
      x.adjustVelocity(vel.toPhys2dVecD)
      x.adjustAngularVelocity(ang_vel.toRad)
      x.setUserData((index, shape))
      x
    }
  }

  def applyCollisionImpulse(impulse: DVec, contactVector: DVec, dt: Double) {
    if (!is_static) {
      val x = impulse * invMass
      acc += x / dt
      dacc += x / dt
      vel += x
      dvel += x


      val y = (invI * (contactVector */ impulse)).toDeg
      ang_acc += y / dt
      d_ang_acc += y / dt
      ang_vel += y
      d_ang_vel += y
    }
  }

  def toImmutableBodyState: BodyState = body.copy(
    mass = mass,
    coord = coord,
    acc = acc,
    vel = vel,
    ang_acc = ang_acc,
    ang_vel = ang_vel,
    ang = ang,
    collisions_dacc = dacc,
    collisions_dvel = dvel,
    collisions_d_ang_acc = d_ang_acc,
    collisions_d_ang_vel = d_ang_vel)

  def copy: MutableBodyState = {
    val mb = new MutableBodyState(toImmutableBodyState)
    mb
  }

  override def toString = s"MutableBodyState($index)"

  def saveData: String = s"$index ${acc.x}:${acc.y} ${vel.x}:${vel.y} ${coord.x}:${coord.y} $ang_acc $ang_vel $ang"

  lazy val polygonShape = shape match {
    case p: PolygonShape => Some(p)
    case _ => None
  }
}
