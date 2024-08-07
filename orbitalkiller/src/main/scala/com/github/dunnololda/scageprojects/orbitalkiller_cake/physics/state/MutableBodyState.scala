package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state

import com.github.dunnololda.scage.ScageLibD.{DVec, _}
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{
  Body => Phys2dBody,
  StaticBody => Phys2dStaticBody
}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.PolygonShape
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.contacts.MutableContact
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.{AABB, Shape}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.Phys2dUtils.DVec2DoublePhys2dVector

import scala.collection.mutable.ArrayBuffer

class MutableBodyState(body: BodyState) {
  var active: Boolean = true

  val init_aabb: AABB = body.aabb
  val restitution: Double = body.restitution

  val staticFriction: Double = body.staticFriction
  val dynamicFriction: Double = body.dynamicFriction
  val is_static: Boolean = body.is_static
  val is_bullet: Boolean = body.is_bullet
  val shape: Shape = body.shape
  val index: Int = body.index
  private val strIndex: String = index.toString

  private var _mass = body.mass

  def mass: Double = _mass

  def mass_=(m: Double): Unit = {
    _mass = m
    _invMass = if (is_static || _mass == 0) 0.0 else 1.0 / _mass
    _I = _mass * shape.wI
    _invI = if (is_static || _I == 0) 0.0 else 1.0 / _I
  }

  private var _invMass = if (is_static || _mass == 0) 0.0 else 1.0 / _mass

  def invMass: Double = _invMass

  private var _I = _mass * shape.wI

  def I: Double = _I

  private var _invI = if (is_static || _I == 0) 0.0 else 1.0 / _I

  def invI: Double = _invI

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
  private var d_ang_acc: Double = 0.0
  private var d_ang_vel: Double = 0.0

  val contacts: ArrayBuffer[MutableContact] = ArrayBuffer[MutableContact]()

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

  var aabb: AABB = shape.aabb(coord, ang)

  def phys2dBody: Phys2dBody = {
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

  def phys2dBodyWithShape(shape: Shape): Phys2dBody = {
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

  def applyCollisionImpulse(impulse: DVec, contactVector: DVec, dt: Double): Unit = {
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
    collisions_d_ang_vel = d_ang_vel
  )

  def copy: MutableBodyState = {
    val mb = new MutableBodyState(toImmutableBodyState)
    mb
  }

  override def toString = s"MutableBodyState($index)"

  def saveData: String =
    s"$index acc=${acc.x}:${acc.y} vel=${vel.x}:${vel.y} coord=${coord.x}:${coord.y} ang_acc=$ang_acc ang_vel=$ang_vel ang=$ang"

  lazy val polygonShape: Option[PolygonShape] = shape match {
    case p: PolygonShape => Some(p)
    case _ => None
  }
}
