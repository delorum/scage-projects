package com.github.dunnololda.scageprojects.orbitalkiller.physics

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.CollisionDetector._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.Space
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.Space._
import com.github.dunnololda.scageprojects.orbitalkiller.vessels.ShipsHolder
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.Factors._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.MathUtils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SystemEvolution(var base_dt: Double = 1.0 / 63,
                      system_center: DVec = DVec.zero,
                      init_tacts: Long = 0,
                      collisions_enabled: Boolean = true) {
  private val mutable_system = mutable.HashMap[Int, MutableSystemPart]()
  private val all_bodies = ArrayBuffer[MutableBodyState]()
  private val mutable_system_helper = new EvolutionHelper(mutable_system)
  private val joints = ArrayBuffer[Joint]()
  var tacts = init_tacts

  private var bullets_counter: Int = 0

  def bulletsInSystem: Boolean = bullets_counter > 0

  def addBody(new_part: MutableSystemPart): Unit = {
    mutable_system += (new_part.body.index -> new_part)
    all_bodies += new_part.body
    if (new_part.body.is_bullet) {
      bullets_counter += 1
    }
  }

  def addBody(b: MutableBodyState,
              force: (Long, EvolutionHelper) => DVec,
              torque: (Long, EvolutionHelper) => Double): Unit = {
    val new_part = MutableSystemPart(b, force, torque)
    addBody(new_part)
  }

  def addBody(b: MutableBodyState): Unit = {
    val new_part = MutableSystemPart(b, (t, h) => DVec.zero, (t, h) => 0.0)
    mutable_system += (b.index -> new_part)
    all_bodies += b
  }

  def addJoint(a: MutableBodyState, vertexA: DVec, b: MutableBodyState, vertexB: DVec): Joint = {
    val j = new Joint(a, vertexA, b, vertexB)
    joints += j
    j
  }

  def removeJoint(j: Joint): Unit = {
    joints -= j
  }

  /**
    *
    */
  private val collision_exclusions = mutable.HashMap[Int, mutable.HashSet[Int]]()

  def addCollisionExclusion(index1: Int, index2: Int): Unit = {
    if (index1 != index2) {
      if (index1 < index2) {
        collision_exclusions.getOrElseUpdate(index1, mutable.HashSet[Int]()) += index2
      } else {
        addCollisionExclusion(index2, index1)
      }
    }
  }

  def removeCollisionExclusion(index1: Int, index2: Int): Unit = {
    if (index1 != index2) {
      if (index1 < index2) {
        collision_exclusions.get(index1).foreach(_ -= index2)
      } else {
        addCollisionExclusion(index2, index1)
      }
    }
  }

  def excludeCollisionCheck(index1: Int, index2: Int): Boolean = {
    index1 != index2 && {
      if (index1 < index2) {
        collision_exclusions.get(index1).exists(_.contains(index2))
      } else {
        excludeCollisionCheck(index2, index1)
      }
    }
  }

  def removeBodyByIndex(index: Int): Unit = {
    mutable_system.remove(index).foreach(part => {
      all_bodies -= part.body
      if (part.body.is_bullet) {
        bullets_counter -= 1
      }
      joints.filter(j => j.a.index == index || j.b.index == index).foreach(j => joints -= j)
      collision_exclusions -= index
      collision_exclusions.foreach(kv => if (kv._2.contains(index)) kv._2 -= index)
    })
  }

  def allBodyStates: mutable.Map[Int, MutableBodyState] = {
    mutable_system.filter(_._2.body.active).map(kv => (kv._1, kv._2.body))
  }

  def bodyStates(indicies: Set[Int]): mutable.Map[Int, MutableBodyState] = {
    mutable_system.filter(kv => kv._2.body.active && indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  }

  def bodyStates(indicies: mutable.Set[Int]): mutable.Map[Int, MutableBodyState] = {
    mutable_system.filter(kv => kv._2.body.active && indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  }

  def bodyState(index: Int): Option[MutableBodyState] = mutable_system.get(index).map(_.body)

  private val already_checked = mutable.HashMap[Int, mutable.HashSet[Int]]()

  private def alreadyChecked(index1: Int, index2: Int): Boolean = {
    index1 != index2 && {
      if (index1 < index2) {
        already_checked.get(index1).exists(_.contains(index2))
      } else {
        alreadyChecked(index2, index1)
      }
    }
  }

  def setAlreadyChecked(index1: Int, index2: Int): Unit = {
    if (index1 != index2) {
      if (index1 < index2) {
        already_checked.getOrElseUpdate(index1, mutable.HashSet[Int]()) += index2
      } else {
        setAlreadyChecked(index2, index1)
      }
    }
  }

  def step(): Unit = {
    val substeps = if (bulletsInSystem) 60 else 1
    val dt = if (bulletsInSystem) base_dt / 60 else base_dt
    (1 to substeps).foreach(_ => {
      all_bodies.foreach(_.init())

      val collisions = if (collisions_enabled) {
        /*already_checked.clear()
        val x = splitSpace(new Space(all_bodies, system_center), 5, 2)
        for {
          space <- x
          if space.bodies.length > 1
          (b1, idx) <- space.bodies.zipWithIndex.init
          b2 <- space.bodies.drop(idx+1)
          if !b1.is_static || !b2.is_static
          if !excludeCollisionCheck(b1.index, b2.index) && !alreadyChecked(b1.index, b2.index)
          c <- {setAlreadyChecked(b1.index, b2.index); maybeCollisions(b1, b2)}
        } yield c*/
        for {
          (b1, idx) <- all_bodies.zipWithIndex.init
          if b1.active
          b2 <- all_bodies.drop(idx + 1)
          if b2.active
          if !b1.is_static || !b2.is_static
          if !excludeCollisionCheck(b1.index, b2.index)
          c <- maybeCollisions(b1, b2)
        } yield c
      } else Nil
      //println("===============")

      // Integrate forces first part
      mutable_system.foreach { case (_, MutableSystemPart(mb, force, torque)) =>
        if (mb.active && !mb.is_static) {
          val next_force = force(tacts, mutable_system_helper)
          val next_acc = next_force * mb.invMass
          val next_torque = torque(tacts, mutable_system_helper)
          val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
          mb.acc = next_acc
          mb.vel += next_acc * dt * 0.5
          mb.ang_acc = next_ang_acc
          mb.ang_vel += next_ang_acc * dt * 0.5
        }
      }

      // Solve collisions
      if (collisions_enabled && collisions.nonEmpty) {
        collisions.foreach(c => c.solveCollision(dt))
      }

      // Solve joints
      if (joints.nonEmpty) joints.foreach(j => j.solveConstraint(dt))

      // Integrate velocities and forces last part
      mutable_system.foreach { case (_, MutableSystemPart(mb, force, torque)) =>
        if (mb.active && !mb.is_static) {
          mb.coord += mb.vel * dt
          mb.ang = correctAngle((mb.ang + mb.ang_vel * dt) % 360)
          mb.aabb = mb.shape.aabb(mb.coord, mb.ang)
          val next_force2 = force(tacts, mutable_system_helper)
          val next_acc2 = next_force2 * mb.invMass
          val next_torque2 = torque(tacts, mutable_system_helper)
          val next_ang_acc2 = (next_torque2 * mb.invI).toDeg // in degrees
          mb.acc += next_acc2
          mb.vel += next_acc2 * dt * 0.5
          mb.ang_acc += next_ang_acc2
          mb.ang_vel += next_ang_acc2 * dt * 0.5
        }
      }

      // Correct positions
      if (collisions_enabled && collisions.nonEmpty) {
        collisions.foreach(c => {
          c.positionalCorrection(tacts)
          ShipsHolder.shipByIndex(c.a.index).foreach(s => s.checkCriticalCollision())
          ShipsHolder.shipByIndex(c.b.index).foreach(s => s.checkCriticalCollision())
        })
      }
    })

    tacts += 1l
  }

  def copy(dt: Double = base_dt, exclude: Set[Int] = Set.empty, collisions_enabled: Boolean = true): SystemEvolution = {
    val x = new SystemEvolution(dt, system_center, tacts, collisions_enabled)
    mutable_system.foreach(p => {
      if (p._2.body.active && !exclude.contains(p._1)) {
        x.addBody(p._2.copy(body = p._2.body.copy))
      }
    })
    x
  }
}

object SystemEvolution {
  def mutableSystemEvolution(mutable_system: Seq[(MutableBodyState, Seq[MutableBodyState])],
                             base_dt: Double, // в секундах
                             force: (MutableBodyState, Seq[MutableBodyState]) => DVec = (body, other_bodies) => DVec.dzero,
                             torque: (MutableBodyState, Seq[MutableBodyState]) => Double = (body, other_bodies) => 0.0,
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
        c <- maybeCollisions(b1, b2)
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

  def systemEvolutionFrom(dt: => Double, // в секундах, может быть больше либо равно base_dt - обеспечивается ускорение времени
                          maxMultiplier: => Int = 1,
                          base_dt: Double, // в секундах
                          force: (Long, BodyState, List[BodyState]) => DVec = (time, body, other_bodies) => DVec.dzero,
                          torque: (Long, BodyState, List[BodyState]) => Double = (time, body, other_bodies) => 0.0,
                          changeFunction: (Long, List[BodyState]) => (Long, List[BodyState]) = (time, bodies) => (time, bodies),
                          enable_collisions: Boolean = true)
                         (current_state: (Long, List[BodyState])): Stream[(Long, List[BodyState])] = {
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
          c <- maybeCollisions(b1, b2)
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
          val next_torque = torque(tacts, b, other_bodies)
          val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
          mb.acc = next_acc
          mb.vel += next_acc * _dt * 0.5
          mb.ang_acc = next_ang_acc
          mb.ang_vel += next_ang_acc * _dt * 0.5
        }
      }

      // Solve collisions
      if (collisions.nonEmpty) collisions.foreach(c => c.solveCollision(_dt))

      // Integrate velocities and forces last part
      mb_and_others.foreach { case (mb2, other_mutable_bodies2) =>
        if (!mb2.is_static) {
          val b2 = mb2.toImmutableBodyState
          mb2.coord += mb2.vel * _dt
          mb2.ang = correctAngle((mb2.ang + mb2.ang_vel * _dt) % 360)
          val other_bodies2 = other_mutable_bodies2.map(_.toImmutableBodyState)
          val next_force2 = force(tacts, b2, other_bodies2)
          val next_acc2 = next_force2 * b2.invMass
          val next_torque2 = torque(tacts, b2, other_bodies2)
          val next_ang_acc2 = (next_torque2 * b2.invI).toDeg // in degrees
          mb2.acc += next_acc2
          mb2.vel += next_acc2 * _dt * 0.5
          mb2.ang_acc += next_ang_acc2
          mb2.ang_vel += next_ang_acc2 * _dt * 0.5
        }
      }

      // Correct positions
      if (collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

      val x = collisions.groupBy(mc => mc.a.index).map(kv => (kv._1, kv._2.map(_.toImmutableForA)))
      val y = collisions.groupBy(mc => mc.b.index).map(kv => (kv._1, kv._2.map(_.toImmutableForB)))
      val z = (x.keySet ++ y.keySet).map(body_index => (body_index, x.getOrElse(body_index, Nil) ::: y.getOrElse(body_index, Nil))).toMap

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
        (1 to steps).foldLeft(current_state) {
          case (state, step) =>
            _step(state, base_dt, 1)
        }
      } else {
        // пусть maxMultiplier = 450, а мы хотим ускорение 1000
        // тогда подбираем ближайший multiplier меньше 450, на который делится 1000 без остатка
        val multiplier = factors5(steps).filter(_ <= max_multiplier).max.toInt
        val steps2 = steps / multiplier
        (1 to steps2).foldLeft(current_state) {
          case (state, step) =>
            _step(state, base_dt * multiplier, multiplier)
        }
      }
    }

    next_state #:: systemEvolutionFrom(dt, maxMultiplier, base_dt, force, torque, changeFunction, enable_collisions)(next_state)
  }
}
