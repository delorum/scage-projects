package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.func

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Collider.findCollisions
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Space
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Space.splitSpace
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.correctAngle

object MutableSystemEvolution {

  def step(
      mutable_system: Seq[(MutableBodyState, Seq[MutableBodyState])],
      base_dt: Double, // в секундах
      force: (MutableBodyState, Seq[MutableBodyState]) => DVec = (_, _) => DVec.dzero,
      torque: (MutableBodyState, Seq[MutableBodyState]) => Double = (_, _) => 0.0,
      enable_collisions: Boolean = true,
      system_center: DVec = DVec.zero): Unit = {
    mutable_system.foreach(_._1.init())

    val collisions = if (enable_collisions) {
      val x = splitSpace(new Space(mutable_system.map(_._1), system_center), 5, 2)
      for {
        space <- x
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx + 1)
        if !b1.is_static || !b2.is_static
        c <- findCollisions(b1, b2)
      } yield c
    } else Nil

    // Integrate forces first part
    mutable_system.foreach { case (mb, other_mutable_bodies) =>
      if (!mb.is_static) {
        val next_force = force(mb, other_mutable_bodies)
        val next_acc = next_force * mb.invMass
        val next_torque = torque(mb, other_mutable_bodies)
        val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
        mb.acc = next_acc
        mb.vel += next_acc * base_dt * 0.5
        mb.ang_acc = next_ang_acc
        mb.ang_vel += next_ang_acc * base_dt * 0.5
      }
    }

    // Solve collisions
    if (collisions.nonEmpty) collisions.foreach(c => c.solveCollision(base_dt))

    // Integrate velocities and forces last part
    mutable_system.foreach { case (mb2, other_mutable_bodies2) =>
      if (!mb2.is_static) {
        mb2.coord += mb2.vel * base_dt
        mb2.ang = correctAngle((mb2.ang + mb2.ang_vel * base_dt) % 360)
        val next_force2 = force(mb2, other_mutable_bodies2)
        val next_acc2 = next_force2 * mb2.invMass
        val next_torque2 = torque(mb2, other_mutable_bodies2)
        val next_ang_acc2 = (next_torque2 * mb2.invI).toDeg // in degrees
        mb2.acc += next_acc2
        mb2.vel += next_acc2 * base_dt * 0.5
        mb2.ang_acc += next_ang_acc2
        mb2.ang_vel += next_ang_acc2 * base_dt * 0.5
      }
    }

    // Correct positions
    if (collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())
  }
}
