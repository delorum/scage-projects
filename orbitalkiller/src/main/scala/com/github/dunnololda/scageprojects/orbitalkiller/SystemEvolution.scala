package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MutableSystemPart(body:MutableBodyState,
                             force:(Long, EvolutionHelper) => DVec,
                             torque:(Long, EvolutionHelper) => Double)

class SystemEvolution(val base_dt:Double = 1.0/63,
                      init_space_center:DVec = DVec.zero,
                      init_tacts:Long = 0) {
  private val mutable_system = mutable.HashMap[String, MutableSystemPart]()
  private val all_bodies = ArrayBuffer[MutableBodyState]()
  private val mutable_system_helper = new EvolutionHelper(mutable_system)
  var tacts = init_tacts

  def addBody(new_part:MutableSystemPart): Unit = {
    mutable_system += (new_part.body.index -> new_part)
    all_bodies += new_part.body
  }

  def addBody(b:MutableBodyState,
              force:(Long, EvolutionHelper) => DVec,
              torque:(Long, EvolutionHelper) => Double): Unit = {
    val new_part = MutableSystemPart(b, force, torque)
    mutable_system += (b.index -> new_part)
    all_bodies += b
  }

  def removeBodyByIndex(index:String): Unit = {
    mutable_system.remove(index).foreach(part => all_bodies -= part.body)
  }

  def allBodyStates:mutable.Map[String, MutableBodyState] = {
    mutable_system.map(kv => (kv._1, kv._2.body))
  }
  def bodyStates(indicies:Set[String]):mutable.Map[String, MutableBodyState] = {
    mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  }
  def bodyState(index:String):Option[MutableBodyState] = mutable_system.get(index).map(_.body)

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
        val next_force = force(tacts, mutable_system_helper)
        val next_acc = next_force * mb.invMass
        val next_torque = torque(tacts, mutable_system_helper)
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
        val next_force2 = force(tacts, mutable_system_helper)
        val next_acc2 = next_force2 * mb.invMass
        val next_torque2 = torque(tacts, mutable_system_helper)
        val next_ang_acc2 = (next_torque2 * mb.invI).toDeg // in degrees
        mb.acc += next_acc2
        mb.vel += next_acc2 * base_dt * 0.5
        mb.ang_acc += next_ang_acc2
        mb.ang_vel += next_ang_acc2 * base_dt * 0.5
      }
    }

    // Correct positions
    if(collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

    tacts += 1l
  }

  def copy(tacts:Long):SystemEvolution = {
    val x = new SystemEvolution(base_dt, init_space_center, tacts)
    mutable_system.foreach(p => x.addBody(p._2.copy(body = p._2.body.copy)))
    x
  }
}

class EvolutionHelper(mutable_system:mutable.HashMap[String, MutableSystemPart]) {
  def gravityForceHelper(planet_index:String, ship_index:String):DVec = {
    (for {
      planet <- mutable_system.get(planet_index).map(_.body)
      ship <- mutable_system.get(ship_index).map(_.body)
    } yield gravityForce(planet.coord, planet.mass, ship.coord, ship.mass, G)).getOrElse(DVec.zero)
  }

  def funcOrDVecZero(body_index:String, func: MutableBodyState => DVec):DVec = {
    mutable_system.get(body_index).map(bs => func(bs.body)).getOrElse(DVec.zero)
  }

  def funcOfArrayOrDVecZero(body_indicies:Array[String], func: Array[MutableBodyState] => DVec):DVec = {
    val bodies = body_indicies.flatMap(idx => mutable_system.get(idx).map(bs => bs.body))
    if(bodies.length == body_indicies.length) func(bodies) else DVec.zero
  }

  def funcOrDoubleZero(body_index:String, func: MutableBodyState => Double):Double = {
    mutable_system.get(body_index).map(bs => func(bs.body)).getOrElse(0.0)
  }

  def bodyStates(indicies:Set[String]) = mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  def bodyState(index:String) = mutable_system.get(index).map(_.body)
}
