package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.{MutableBodyState, correctAngle, maybeCollisions}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SystemEvolution(
    var base_dt: Double = 1.0 / 63,
    system_center: DVec = DVec.zero,
    init_tacts: Long = 0,
    collisions_enabled: Boolean = true) {
  private val mutable_system = mutable.HashMap[Int, MutableSystemPart]()
  private val all_bodies = ArrayBuffer[MutableBodyState]()
  private val mutable_system_helper = new EvolutionHelper(mutable_system)
  private val joints = ArrayBuffer[Joint]()

  private var _tacts: Long = init_tacts

  def tacts: Long = _tacts

  def setTacts(newTacts: Long): Unit = {
    _tacts = newTacts
  }

  def timeMsec: Long = (tacts * base_dt * 1000).toLong

  private var bullets_counter: Int = 0

  private def bulletsInSystem: Boolean = bullets_counter > 0

  private def addBody(new_part: MutableSystemPart): Unit = {
    mutable_system += (new_part.body.index -> new_part)
    all_bodies += new_part.body
    if (new_part.body.is_bullet) {
      bullets_counter += 1
    }
  }

  def addBody(
      b: MutableBodyState,
      force: (Long, EvolutionHelper) => DVec = (_, _) => DVec.zero,
      torque: (Long, EvolutionHelper) => Double = (_, _) => 0.0,
      onCollision: () => Unit = () => ()): Unit = {
    val new_part = MutableSystemPart(b, force, torque, onCollision)
    addBody(new_part)
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

  @tailrec
  private def excludeCollisionCheck(index1: Int, index2: Int): Boolean = {
    index1 != index2 && {
      if (index1 < index2) {
        collision_exclusions.get(index1).exists(_.contains(index2))
      } else {
        excludeCollisionCheck(index2, index1)
      }
    }
  }

  def removeBodyByIndex(index: Int): Unit = {
    mutable_system
      .remove(index)
      .foreach(part => {
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
      // println("===============")

      // Integrate forces first part
      mutable_system.foreach { case (_, MutableSystemPart(mb, force, torque, _)) =>
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
      mutable_system.foreach { case (_, MutableSystemPart(mb, force, torque, _)) =>
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
          mutable_system.get(c.a.index).foreach(_.onCollision())
          mutable_system.get(c.b.index).foreach(_.onCollision())
        })
      }
    })

    _tacts += 1L
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
