package com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.CircleShape
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.BodyState

class Star(
    val index: Int,
    val name: String,
    val mass: Double,
    val coord: DVec,
    /*val init_ang_vel:Double,*/
    val radius: Double)
  extends CelestialBody {

  def initState: BodyState = BodyState(
    index = index,
    mass = mass,
    acc = DVec.dzero,
    vel = DVec.dzero,
    coord = coord,
    ang_acc = 0,
    ang_vel = 0,
    ang = 0,
    shape = CircleShape(radius),
    is_static = false
  )

  def linearVelocity: DVec = DVec.dzero

  val half_hill_radius: Double = 0.0
  val air_free_altitude: Double = 0.0
}
