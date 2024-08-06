package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.stream

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Collider.findCollisions
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Space
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Space.splitSpace
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.BodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.correctAngle
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.factors.Factors.factors5

object SystemEvolutionStream {

  def from(
      dt: => Double, // в секундах, может быть больше либо равно base_dt - обеспечивается ускорение времени
      maxMultiplier: => Int = 1,
      base_dt: Double, // в секундах
      force: (Long, BodyState, List[BodyState]) => DVec = (_, _, _) => DVec.dzero,
      torque: (Long, BodyState, List[BodyState]) => Double = (_, _, _) => 0.0,
      changeFunction: (Long, List[BodyState]) => (Long, List[BodyState]) = (time, bodies) => (time, bodies),
      enable_collisions: Boolean = true
    )(current_state: (Long, List[BodyState])): Stream[(Long, List[BodyState])] = {
    def _step(state: (Long, List[BodyState]), _dt: Double, steps: Int): (Long, List[BodyState]) = {
      val (tacts, bodies) = changeFunction(state._1, state._2)
      val next_tacts = tacts + steps

      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-the-basics-and-impulse-resolution--gamedev-6331
      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-friction-scene-and-jump-table--gamedev-7756
      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-oriented-rigid-bodies--gamedev-8032
      // http://www.niksula.hut.fi/~hkankaan/Homepages/gravity.html

      val mutable_bodies = bodies.map(_.toMutableBodyState)

      val collisions = if (enable_collisions && _dt == base_dt) {
        for {
          space <- splitSpace(new Space(mutable_bodies, DVec.zero), 5, 3)
          if space.bodies.length > 1
          (b1, idx) <- space.bodies.zipWithIndex.init
          b2 <- space.bodies.drop(idx + 1)
          if !b1.is_static || !b2.is_static
          c <- findCollisions(b1, b2)
        } yield c
      } else Nil

      val mb_and_others = for {
        (mb, idx) <- mutable_bodies.zipWithIndex
        other_mutable_bodies = mutable_bodies.take(idx) ::: mutable_bodies.drop(idx + 1)
      } yield (mb, other_mutable_bodies)

      // Integrate forces first part
      mb_and_others.foreach { case (mb, other_mutable_bodies) =>
        if (!mb.is_static) {
          val b = mb.toImmutableBodyState
          val other_bodies = other_mutable_bodies.map(_.toImmutableBodyState)

          val next_force = force(tacts, b, other_bodies)
          val next_acc = next_force * mb.invMass
          mb.acc = next_acc
          mb.vel += mb.acc * _dt * 0.5

          val next_torque = torque(tacts, b, other_bodies)
          val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
          mb.ang_acc = next_ang_acc
          mb.ang_vel += mb.ang_acc * _dt * 0.5
        }
      }

      // Solve collisions
      if (collisions.nonEmpty) {
        collisions.foreach(c => c.solveCollision(_dt))
      }

      // Integrate velocities and forces last part
      mb_and_others.foreach { case (mb2, other_mutable_bodies2) =>
        if (!mb2.is_static) {
          val b2 = mb2.toImmutableBodyState
          mb2.coord += mb2.vel * _dt
          mb2.ang = correctAngle((mb2.ang + mb2.ang_vel * _dt) % 360)
          val other_bodies2 = other_mutable_bodies2.map(_.toImmutableBodyState)

          val next_force2 = force(tacts, b2, other_bodies2)
          val next_acc2 = next_force2 * b2.invMass
          mb2.acc = next_acc2
          mb2.vel += mb2.acc * _dt * 0.5

          val next_torque2 = torque(tacts, b2, other_bodies2)
          val next_ang_acc2 = (next_torque2 * b2.invI).toDeg // in degrees
          mb2.ang_acc = next_ang_acc2
          mb2.ang_vel += mb2.ang_acc * _dt * 0.5
        }
      }

      // Correct positions
      if (collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

      val x = collisions.groupBy(mc => mc.a.index).map(kv => (kv._1, kv._2.map(_.toImmutableForA)))
      val y = collisions.groupBy(mc => mc.b.index).map(kv => (kv._1, kv._2.map(_.toImmutableForB)))
      val z = (x.keySet ++ y.keySet)
        .map(body_index => (body_index, x.getOrElse(body_index, Nil) ::: y.getOrElse(body_index, Nil)))
        .toMap

      val next_bodies = mutable_bodies.map(mb => mb.toImmutableBodyState.copy(collisions = z.getOrElse(mb.index, Nil)))

      (next_tacts, next_bodies)
    }
    val cur_dt = dt
    val max_multiplier = maxMultiplier
    val steps = math.max((cur_dt / base_dt).toInt, 1)

    val next_state = if (steps < max_multiplier) {
      _step(current_state, cur_dt, steps)
    } else {
      if (max_multiplier == 1) {
        (1 to steps).foldLeft(current_state) { case (state, _) =>
          _step(state, base_dt, 1)
        }
      } else {
        // пусть maxMultiplier = 450, а мы хотим ускорение 1000
        // тогда подбираем ближайший multiplier меньше 450, на который делится 1000 без остатка
        val multiplier = factors5(steps).filter(_ <= max_multiplier).max.toInt
        val steps2 = steps / multiplier
        (1 to steps2).foldLeft(current_state) { case (state, _) =>
          _step(state, base_dt * multiplier, multiplier)
        }
      }
    }

    next_state #:: from(dt, maxMultiplier, base_dt, force, torque, changeFunction, enable_collisions)(next_state)
  }
}
