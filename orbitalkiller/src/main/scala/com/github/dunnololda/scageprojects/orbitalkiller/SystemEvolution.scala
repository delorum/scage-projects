package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SixDimVector(val a1:Double, val a2:Double, val a3:Double, val a4:Double, val a5:Double, val a6:Double) {
  private val l = List(a1,a2,a3,a4,a5,a6)
  def this(l:Seq[Double]) = this(l.head, l(1), l(2), l(3), l(4), l(5))
  def *(k:Double):SixDimVector = new SixDimVector(l.map(_ * k))
  def *(p:SixDimVector):Double = l.zip(p.l).map(kv => kv._1*kv._2).sum
  def **(p:SixDimVector):SixDimVector = new SixDimVector(l.zip(p.l).map(kv => kv._1*kv._2))
  def +(p:SixDimVector):SixDimVector = new SixDimVector(l.zip(p.l).map(kv => kv._1+kv._2))

  /*val bVel = DVec(a1, a2)
  val bAngVelRad = a3
  val aVel = DVec(a4, a5)
  val aAngVelRad = a6*/
}

// http://myselph.de/gamePhysics/equalityConstraints.html
class Joint (a:MutableBodyState, vertexA:DVec, b:MutableBodyState, vertexB:DVec) {
  def solveConstraint(_dt:Double) {
    val MInv = new SixDimVector(b.invMass, b.invMass, b.invI, a.invMass, a.invMass, a.invI)
    val pA = a.coord + vertexA.rotateDeg(a.ang)
    val cA = a.coord
    val pB = b.coord + vertexB.rotateDeg(b.ang)
    val cB = b.coord
    val J = {
      val e1 = pB - pA
      val e2 = (pA - pB)*/(pB - cB)
      val e3 = pA - pB
      val e4 = (pB - pA)*/(pA - cA)
      new SixDimVector(e1.x, e1.y, e2, e3.x, e3.y, e4)*2
    }
    val C = (pA - pB)*(pA - pB)
    val bias = 0.2 / _dt * C
    (1 to 4).foreach(iteration => {
      val v = new SixDimVector(b.vel.x, b.vel.y, b.ang_vel.toRad, a.vel.x, a.vel.y, a.ang_vel.toRad)
      val lambdaDenominator = J*(MInv**J)
      if(math.abs(lambdaDenominator) > 1E-15) {
        val lambda = -(J*v + bias) / lambdaDenominator
        val new_v = v + (MInv**(J*lambda))
        a.vel = DVec(new_v.a4, new_v.a5)
        a.ang_vel = new_v.a6.toDeg
        b.vel = DVec(new_v.a1, new_v.a2)
        b.ang_vel = new_v.a3.toDeg
      }
    })
  }
}

case class MutableSystemPart(body:MutableBodyState,
                             force:(Long, EvolutionHelper) => DVec,
                             torque:(Long, EvolutionHelper) => Double)

class SystemEvolution(val base_dt:Double = 1.0/63,
                      system_center:DVec = DVec.zero,
                      init_tacts:Long = 0) {
  private val mutable_system = mutable.HashMap[Int, MutableSystemPart]()
  private val all_bodies = ArrayBuffer[MutableBodyState]()
  private val mutable_system_helper = new EvolutionHelper(mutable_system)
  private val joints = ArrayBuffer[Joint]()
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

  def addBody(b:MutableBodyState): Unit = {
     val new_part = MutableSystemPart(b, (t, h) => DVec.zero, (t, h) => 0.0)
    mutable_system += (b.index -> new_part)
    all_bodies += b
  }

  def addJoint(a:MutableBodyState, vertexA:DVec, b:MutableBodyState, vertexB:DVec):Joint = {
    val j = new Joint(a, vertexA, b, vertexB)
    joints += j
    j
  }

  def removeJoint(j:Joint): Unit = {
    joints -= j
  }

  /**
   *
   */
  private val collision_exclusions = mutable.HashMap[Int, mutable.HashSet[Int]]()
  def addCollisionExclusion(index1:Int, index2:Int): Unit = {
    if(index1 != index2) {
      if(index1 < index2) {
        collision_exclusions.getOrElseUpdate(index1, mutable.HashSet[Int]()) += index2
      } else {
        addCollisionExclusion(index2, index1)
      }
    }
  }
  def removeCollisionExclusion(index1:Int, index2:Int): Unit = {
    if(index1 != index2) {
      if(index1 < index2) {
        collision_exclusions.get(index1).foreach(_ -= index2)
      } else {
        addCollisionExclusion(index2, index1)
      }
    }
  }
  def excludeCollisionCheck(index1:Int, index2:Int):Boolean = {
    index1 != index2 && {
      if(index1 < index2) {
        collision_exclusions.get(index1).exists(_.contains(index2))
      } else {
        excludeCollisionCheck(index2, index1)
      }
    }
  }

  def removeBodyByIndex(index:Int): Unit = {
    mutable_system.remove(index).foreach(part => all_bodies -= part.body)
  }

  def allBodyStates:mutable.Map[Int, MutableBodyState] = {
    mutable_system.map(kv => (kv._1, kv._2.body))
  }
  def bodyStates(indicies:Set[Int]):mutable.Map[Int, MutableBodyState] = {
    mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  }
  def bodyState(index:Int):Option[MutableBodyState] = mutable_system.get(index).map(_.body)

  def step(): Unit = {
    mutable_system.foreach(_._2.body.init())

    val collisions = {
      val x = splitSpace(new Space(all_bodies, system_center), 5, 2)
      for {
        space <- x
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx+1)
        if !excludeCollisionCheck(b1.index, b2.index)
        if !b1.is_static || !b2.is_static
        c <- maybeCollisions(b1, b2)
      } yield c
    }
    //println("=================")

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

    // Solve joints
    joints.foreach(j => j.solveConstraint(base_dt))

    // Integrate velocities and forces last part
    mutable_system.foreach{ case (index, MutableSystemPart(mb, force, torque)) =>
      if(!mb.is_static) {
        if(mb.is_bullet) {
          val other_bodies = mutable_system.filterNot(_._1 == mb.index).values.toList
          println("==================")
          def _increaseCoord(cur_count:Int, num_counts:Int): Unit = {
            if(cur_count > 0) {
              println(mOrKmOrMKm(mb.vel.dist(OrbitalKiller.ship.linearVelocity) * base_dt / num_counts))
              mb.coord += mb.vel * base_dt / num_counts
              mb.ang = correctAngle((mb.ang + mb.ang_vel*base_dt/num_counts) % 360)
              mb.aabb = mb.shape.aabb(mb.coord, mb.ang)
              if (other_bodies.forall(ob => {
                val res = maybeCollisions(mb, ob.body).isEmpty
                if(!res) {
                  println(s"${OrbitalKiller.nameByIndex(mb.index).getOrElse("N/A")} <-> ${OrbitalKiller.nameByIndex(ob.body.index).getOrElse("N/A")}")
                }
                res
              })) {
                _increaseCoord(cur_count - 1, num_counts)
              }
            }
          }
          _increaseCoord(20, 20)
          println("==================")
        } else {
          mb.coord += mb.vel * base_dt
          mb.ang = correctAngle((mb.ang + mb.ang_vel*base_dt) % 360)
        }
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

  def copy:SystemEvolution = {
    val x = new SystemEvolution(base_dt, system_center, tacts)
    mutable_system.foreach(p => x.addBody(p._2.copy(body = p._2.body.copy)))
    x
  }
}

class EvolutionHelper(mutable_system:mutable.HashMap[Int, MutableSystemPart]) {
  def gravityForceFromTo(planet_index:Int, ship_index:Int):DVec = {
    (for {
      planet <- mutable_system.get(planet_index).map(_.body)
      ship <- mutable_system.get(ship_index).map(_.body)
    } yield gravityForce(planet.coord, planet.mass, ship.coord, ship.mass, G)).getOrElse(DVec.zero)
  }

  def funcOrDVecZero(body_index:Int, func: MutableBodyState => DVec):DVec = {
    mutable_system.get(body_index).map(bs => func(bs.body)).getOrElse(DVec.zero)
  }

  def funcOfArrayOrDVecZero(body_indicies:Array[Int], func: Array[MutableBodyState] => DVec):DVec = {
    val bodies = body_indicies.flatMap(idx => mutable_system.get(idx).map(bs => bs.body))
    if(bodies.length == body_indicies.length) func(bodies) else DVec.zero
  }

  def funcOrDoubleZero(body_index:Int, func: MutableBodyState => Double):Double = {
    mutable_system.get(body_index).map(bs => func(bs.body)).getOrElse(0.0)
  }

  def bodyStates(indicies:Set[Int]) = mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  def bodyState(index:Int) = mutable_system.get(index).map(_.body)
}
