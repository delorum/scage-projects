package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.contacts

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState

case class MutableContact(a: MutableBodyState, b: MutableBodyState, contact_point: DVec, normal: DVec, separation: Double) {
  def solveCollision(_dt: Double): Unit = {
    a.contacts += this
    b.contacts += this
    /*val a_prev_vel = a.vel
    val b_prev_vel = b.vel*/

    val e = /*1*//*0.9*/ math.min(a.restitution, b.restitution)

    // radii from centers of masses to contact
    val ra = contact_point - a.coord
    val rb = contact_point - b.coord

    val rv = b.vel + (b.ang_vel.toRad */ rb) - a.vel - (a.ang_vel.toRad */ ra) // Relative velocity

    val contactVel = rv * normal // Relative velocity along the normal
    if (contactVel <= 0) {
      // Do not resolve if velocities are separating
      val raCrossN = ra */ normal
      val rbCrossN = rb */ normal

      val invMassSum = a.invMass + b.invMass + (raCrossN * raCrossN) * a.invI + (rbCrossN * rbCrossN) * b.invI
      val j = (-(1.0 + e) * contactVel) / invMassSum ///contact_points.length
      val impulse = normal * j
      a.applyCollisionImpulse(-impulse, ra, _dt)
      b.applyCollisionImpulse(impulse, rb, _dt)

      val t = (rv + normal * (-rv * normal)).n
      val jt = (-(rv * t)) / invMassSum ///contact_points.length
      if (math.abs(jt) > 0.0001) {
        // Coulumb's law
        val tangentImpulse = if (math.abs(jt) < j * a.staticFriction) {
          t * jt
        } else {
          t * j * (-a.dynamicFriction)
        }
        a.applyCollisionImpulse(-tangentImpulse, ra, _dt)
        b.applyCollisionImpulse(tangentImpulse, rb, _dt)
      }
    }

    /*if(!a.body.is_static) {
      println(s"скорость ${a.body.index} до столкновения: ${a_prev_vel.norma}, скорость после столкновения: ${a.vel.norma}")
    }
    if(!b.body.is_static) {
      println(s"скорость ${b.body.index} до столкновения: ${b_prev_vel.norma}, скорость после столкновения: ${b.vel.norma}")
    }*/
  }

  def positionalCorrection(tacts: Long = 0): Unit = {
    if (separation > 0.005) {
      val correction = separation / (a.invMass + b.invMass)
      if (correction != 0) {
        println(s"$tacts correction: separation=$separation correction_${a.index}=${-a.invMass * correction} correction_${b.index}=${b.invMass * correction}")
      }
      if (!a.is_static) a.coord += normal * (-a.invMass * correction)
      if (!b.is_static) b.coord += normal * b.invMass * correction
    }
  }

  def toImmutableForA: Contact = Contact(a.toImmutableBodyState,
    b.toImmutableBodyState,
    contact_point,
    normal,
    separation)

  def toImmutableForB: Contact = Contact(b.toImmutableBodyState,
    a.toImmutableBodyState,
    contact_point,
    -normal,
    separation)
}
