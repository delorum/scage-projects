package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution

import com.github.dunnololda.scage.ScageLibD.Double2Vecrich
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.MutableBodyState

// http://myselph.de/gamePhysics/equalityConstraints.html
// http://gamedevelopment.tutsplus.com/tutorials/modelling-and-solving-physical-constraints--gamedev-12578
// http://www.bulletphysics.com/ftp/pub/test/physics/papers/IterativeDynamics.pdf
class Joint(val a: MutableBodyState, val vertexA: DVec, val b: MutableBodyState, val vertexB: DVec) {
  def solveConstraint(_dt: Double): Unit = {
    val MInv = new SixDimVector(b.invMass, b.invMass, b.invI, a.invMass, a.invMass, a.invI)
    val pA = a.coord + vertexA.rotateDeg(a.ang)
    val cA = a.coord
    val pB = b.coord + vertexB.rotateDeg(b.ang)
    val cB = b.coord
    val J = {
      val e1 = pB - pA
      val e2 = (pA - pB) */ (pB - cB)
      val e3 = pA - pB
      val e4 = (pB - pA) */ (pA - cA)
      new SixDimVector(e1.x, e1.y, e2, e3.x, e3.y, e4) * 2
    }
    val C = (pA - pB) * (pA - pB)
    val bias = 0.2 / _dt * C
    /*val prev_a_vel = a.vel
    val prev_a_ang_vel = a.ang_vel
    val prev_b_vel = b.vel
    val prev_b_ang_vel = b.ang_vel*/
    (1 to 4).foreach(_ => {
      val v = new SixDimVector(b.vel.x, b.vel.y, b.ang_vel.toRad, a.vel.x, a.vel.y, a.ang_vel.toRad) // previous state
      val lambdaDenominator = J * (MInv ** J)
      if (math.abs(lambdaDenominator) > 1E-15) {
        val lambda = -(J * v + bias) / lambdaDenominator // the magnitude of the constraint impulse
        val new_v = v + (MInv ** (J * lambda))
        a.vel = DVec(new_v.a4, new_v.a5)
        a.ang_vel = new_v.a6.toDeg
        b.vel = DVec(new_v.a1, new_v.a2)
        b.ang_vel = new_v.a3.toDeg
      }
    })
    /*println(f"${(a.vel - prev_a_vel)*(b.coord - a.coord).n}%.5f : ${(b.vel - prev_b_vel)*(b.coord - a.coord).n}%.5f")
    println(f"${(a.vel - prev_a_vel)*(b.coord - a.coord).p}%.5f : ${(b.vel - prev_b_vel)*(b.coord - a.coord).p}%.5f")
    println(f"${a.ang_vel - prev_a_ang_vel}%.5f : ${b.ang_vel - prev_b_ang_vel}%.5f")*/
  }

  private val m_distance: Double = b.coord.dist(a.coord)

  def solveConstraint2(_dt: Double): Unit = {
    val axis = b.coord - a.coord
    val currentDistance = axis.norma
    val unitAxis = axis/currentDistance
    val relVel = (b.vel - a.vel)*unitAxis
    val relDist = currentDistance - m_distance
    val remove = relVel+relDist/_dt
    val impulse = remove / (a.invMass + b.invMass)
    val I = unitAxis*impulse
    a.vel = a.vel + ( I * a.invMass )
    b.vel = b.vel - ( I * b.invMass )
  }
}
