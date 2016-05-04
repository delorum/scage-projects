package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{ScageId, DVec}

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
// http://gamedevelopment.tutsplus.com/tutorials/modelling-and-solving-physical-constraints--gamedev-12578
// http://www.bulletphysics.com/ftp/pub/test/physics/papers/IterativeDynamics.pdf
class Joint (val a:MutableBodyState, val vertexA:DVec, val b:MutableBodyState, val vertexB:DVec) {
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
    val prev_a_vel = a.vel
    val prev_a_ang_vel = a.ang_vel
    val prev_b_vel = b.vel
    val prev_b_ang_vel = b.ang_vel
    (1 to 4).foreach(iteration => {
      val v = new SixDimVector(b.vel.x, b.vel.y, b.ang_vel.toRad, a.vel.x, a.vel.y, a.ang_vel.toRad)  // previous state
      val lambdaDenominator = J*(MInv**J)
      if(math.abs(lambdaDenominator) > 1E-15) {
        val lambda = -(J*v + bias) / lambdaDenominator // the magnitude of the constraint impulse
        val new_v = v + (MInv**(J*lambda))
        a.vel = DVec(new_v.a4, new_v.a5)
        a.ang_vel = new_v.a6.toDeg
        b.vel = DVec(new_v.a1, new_v.a2)
        b.ang_vel = new_v.a3.toDeg
      }
    })

    println(f"a before: ${prev_a_vel.x}%.5f:${prev_a_vel.y}%.5f after: ${a.vel.x}%.5f:${a.vel.y}%.5f")
    println(f"b before: ${prev_b_vel.x}%.5f:${prev_b_vel.y}%.5f after: ${b.vel.x}%.5f:${b.vel.y}%.5f")
    /*println(f"${(a.vel - prev_a_vel)*(b.coord - a.coord).n}%.10f : ${(b.vel - prev_b_vel)*(b.coord - a.coord).n}%.10f")
    println(f"${(a.vel - prev_a_vel)*(b.coord - a.coord).p}%.10f : ${(b.vel - prev_b_vel)*(b.coord - a.coord).p}%.10f")*/
    println(f"${(a.vel - prev_a_vel).x}%.10f : ${(b.vel - prev_b_vel).x}%.10f")
    println(f"${(a.vel - prev_a_vel).y}%.10f : ${(b.vel - prev_b_vel).y}%.10f")
    println(f"${((a.vel - prev_a_vel) + (b.vel - prev_b_vel)).norma}%.10f")
    println(f"${a.ang_vel - prev_a_ang_vel}%.10f : ${b.ang_vel - prev_b_ang_vel}%.10f")
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

  private var bullets_counter:Int = 0
  def bulletsInSystem:Boolean = bullets_counter > 0

  def addBody(new_part:MutableSystemPart): Unit = {
    mutable_system += (new_part.body.index -> new_part)
    all_bodies += new_part.body
    if(new_part.body.is_bullet) {
      bullets_counter += 1
    }
  }

  def addBody(b:MutableBodyState,
              force:(Long, EvolutionHelper) => DVec,
              torque:(Long, EvolutionHelper) => Double): Unit = {
    val new_part = MutableSystemPart(b, force, torque)
    addBody(new_part)
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
    mutable_system.remove(index).foreach(part => {
      all_bodies -= part.body
      if(part.body.is_bullet) {
        bullets_counter -= 1
      }
      joints.filter(j => j.a.index == index || j.b.index == index).foreach(j => joints -= j)
      collision_exclusions -= index
      collision_exclusions.foreach(kv => if(kv._2.contains(index)) kv._2 -= index)
    })
  }

  def allBodyStates:mutable.Map[Int, MutableBodyState] = {
    mutable_system.map(kv => (kv._1, kv._2.body))
  }
  def bodyStates(indicies:Set[Int]):mutable.Map[Int, MutableBodyState] = {
    mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  }
  def bodyState(index:Int):Option[MutableBodyState] = mutable_system.get(index).map(_.body)

  def step(): Unit = {
    val substeps = if(bulletsInSystem) 60 else 1
    val dt = if(bulletsInSystem) base_dt/60 else base_dt
    (1 to substeps).foreach(_ => {
      mutable_system.foreach(_._2.body.init())

      val collisions = {
        /*val x = splitSpace(new Space(all_bodies, system_center), 5, 2)
        for {
          space <- x
          if space.bodies.length > 1
          (b1, idx) <- space.bodies.zipWithIndex.init
          b2 <- space.bodies.drop(idx+1)
          if !excludeCollisionCheck(b1.index, b2.index)
          if !b1.is_static || !b2.is_static
          c <- maybeCollisions(b1, b2)
        } yield c*/
        for {
          (b1, idx) <- all_bodies.zipWithIndex.init
          b2 <- all_bodies.drop(idx+1)
          if !excludeCollisionCheck(b1.index, b2.index)
          if !b1.is_static || !b2.is_static
          c <- maybeCollisions(b1, b2)
        } yield c
      }
      println("===============")

      // Integrate forces first part
      mutable_system.foreach {case (_, MutableSystemPart(mb, force, torque)) =>
        if(!mb.is_static) {
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
      if(collisions.nonEmpty) {
        collisions.foreach(c => {
          // если одно или оба тела, вошедшие в соприкосновение, являются частью систем с ограничением на расстояние (входят в joint'ы),
          // то надо считать коллизию виртуального тела, составляющего совокупность всей системы
          def _modBody(x:MutableBodyState):Option[(MutableBodyState, Joint)] = {
            joints.find(j => j.a.index == x.index || j.b.index == x.index) match {
              case Some(j) =>
                val mc_coord = (j.a.mass*j.a.coord + j.b.mass*j.b.coord)/(j.a.mass + j.b.mass)
                val mc_vel = (j.a.mass*j.a.vel + j.b.mass*j.b.vel)/(j.a.mass + j.b.mass)
                val mc_I = j.a.I + j.a.mass*mc_coord.dist2(j.a.coord) + j.b.I + j.b.mass*mc_coord.dist2(j.b.coord)
                val ang_vel1 = ((j.a.vel*(j.a.coord - mc_coord).p)/j.a.coord.dist(mc_coord)).toDeg
                val ang_vel2 = ((j.b.vel*(j.b.coord - mc_coord).p)/j.b.coord.dist(mc_coord)).toDeg
                val ang_vel3 = (((j.a.I + j.a.mass*mc_coord.dist2(j.a.coord))*j.a.ang_vel.toRad + (j.b.I + j.b.mass*mc_coord.dist2(j.b.coord))*j.b.ang_vel.toRad)/mc_I).toDeg
                println(f"angles: $ang_vel1%.5f : $ang_vel2%.5f : $ang_vel3%.5f")
                Some((new MutableBodyState(BodyState(
                  index = ScageId.nextId,
                  mass = j.a.mass + j.b.mass,
                  coord = mc_coord,
                  vel = mc_vel,
                  ang_vel = ang_vel3,
                  shape = j.a.shape,
                  restitution = math.min(j.a.restitution, j.b.restitution),
                  staticFriction = 0,
                  dynamicFriction = 0
                )) {
                  _I = mc_I
                  _invI = 1.0/_I
                }, j))
              case None => None
            }
          }
          def _updateBodies(j:Joint, mc:MutableBodyState): Unit = {
            j.a.vel = mc.vel + mc.ang_vel.toRad*j.a.coord.dist(mc.coord)*(j.a.coord - mc.coord).p
            j.b.vel = mc.vel + mc.ang_vel.toRad*j.b.coord.dist(mc.coord)*(j.b.coord - mc.coord).p
          }
          _modBody(c.a) match {
            case Some((mod_a, j_a)) =>
              _modBody(c.b) match {
                case Some((mod_b, j_b)) =>
                  val mod_c = new MutableContact(mod_a, mod_b, c.contact_point, c.normal, c.separation)
                  mod_c.solveCollision(dt)
                  _updateBodies(j_a, mod_a)
                  _updateBodies(j_b, mod_b)
                case None =>
                  val mod_c = new MutableContact(mod_a, c.b, c.contact_point, c.normal, c.separation)
                  mod_c.solveCollision(dt)
                  _updateBodies(j_a, mod_a)
              }
            case None =>
              _modBody(c.b) match {
                case Some((mod_b, j_b)) =>
                  val mod_c = new MutableContact(c.a, mod_b, c.contact_point, c.normal, c.separation)
                  mod_c.solveCollision(dt)
                  _updateBodies(j_b, mod_b)
                case None =>
                  c.solveCollision(dt)
              }
          }
        })
      }

      // Solve joints
      if(joints.nonEmpty) joints.foreach(j => j.solveConstraint(dt))

      // Integrate velocities and forces last part
      mutable_system.foreach{ case (_, MutableSystemPart(mb, force, torque)) =>
        if(!mb.is_static) {
          mb.coord += mb.vel * dt
          mb.ang = correctAngle((mb.ang + mb.ang_vel*dt) % 360)
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
      if(collisions.nonEmpty) {
        collisions.foreach(c => {
          c.positionalCorrection(tacts)
          ShipsHolder.shipByIndex(c.a.index).foreach(s => s.onCollision())
          ShipsHolder.shipByIndex(c.b.index).foreach(s => s.onCollision())
        })
      }
    })

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

  def bodyStates(indicies:collection.Set[Int]) = mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => kv._2.body).toSeq
  def bodyStatesMap(indicies:Set[Int]) = mutable_system.filter(kv => indicies.contains(kv._1)).map(kv => (kv._1, kv._2.body))
  def bodyState(index:Int) = mutable_system.get(index).map(_.body)
}
