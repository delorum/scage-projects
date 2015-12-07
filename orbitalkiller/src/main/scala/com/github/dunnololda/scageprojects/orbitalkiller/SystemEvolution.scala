package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MutableSystemPart(body:MutableBodyState,
                             force:(Long, Map[String, MutableSystemPart]) => DVec,
                             torque:(Long, Map[String, MutableSystemPart]) => Double)

class SystemEvolution(val base_dt:Double = 1.0/63,
                      init_space_center:DVec = DVec.zero,
                      init_tacts:Long = 0) {
  private val mutable_system = mutable.HashMap[String, MutableSystemPart]()
  private val all_bodies = ArrayBuffer[MutableBodyState]()
  private var mutable_system_map = mutable_system.toMap
  private var _tacts = init_tacts

  def addBody(b:MutableBodyState,
              force:(Long, Map[String, MutableSystemPart]) => DVec,
              torque:(Long, Map[String, MutableSystemPart]) => Double): Unit = {
    val new_part = MutableSystemPart(b, force, torque)
    mutable_system += (b.index -> new_part)
    all_bodies += b
    mutable_system_map = mutable_system.toMap
  }

  def removeBodyByIndex(index:String): Unit = {
    mutable_system.remove(index).foreach(part => all_bodies -= part.body)
    mutable_system_map = mutable_system.toMap
  }

  def tacts:Long = _tacts

  def step(): Unit = {
    mutable_system.foreach(_._2.body.init())

    val collisions = {
      val x = splitSpace(new Space(all_bodies, init_space_center), 5, 2)
      for {
        space <- x
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx+1)
        if !b1.is_static || !b2.is_static
        c <- maybeCollisions(b1, b2)
      } yield c
    }

    // Integrate forces first part
    mutable_system.foreach {case (index, MutableSystemPart(mb, force, torque)) =>
      if(!mb.is_static) {
        val next_force = force(_tacts, mutable_system_map)
        val next_acc = next_force * mb.invMass
        val next_torque = torque(_tacts, mutable_system_map)
        val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
        mb.acc = next_acc
        mb.vel += next_acc * base_dt * 0.5
        mb.ang_acc = next_ang_acc
        mb.ang_vel += next_ang_acc * base_dt * 0.5
      }
    }

    // Solve collisions
    if(collisions.nonEmpty) collisions.foreach(c => c.solveCollision(base_dt))

    // Integrate velocities and forces last part
    mutable_system.foreach{ case (index, MutableSystemPart(mb, force, torque)) =>
      if(!mb.is_static) {
        mb.coord += mb.vel*base_dt
        mb.ang = correctAngle((mb.ang + mb.ang_vel*base_dt) % 360)
        val next_force2 = force(_tacts, mutable_system_map)
        val next_acc2 = next_force2 * mb.invMass
        val next_torque2 = torque(_tacts, mutable_system_map)
        val next_ang_acc2 = (next_torque2 * mb.invI).toDeg // in degrees
        mb.acc += next_acc2
        mb.vel += next_acc2 * base_dt * 0.5
        mb.ang_acc += next_ang_acc2
        mb.ang_vel += next_ang_acc2 * base_dt * 0.5
      }
    }

    // Correct positions
    if(collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

    _tacts += 1l
  }
}
