package com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials

import com.github.dunnololda.scage.ScageLibD.{DVec, Double2Vecrich}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.{BodyState, MutableBodyState}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.render.orbits.OrbitRenderData
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics.GravityUtils.G

trait CelestialBody {
  def index: Int

  def name: String

  def coord: DVec

  def linearVelocity: DVec

  def mass: Double

  def radius: Double
  lazy val radius2: Double = radius * radius

  def initState: BodyState

  def half_hill_radius: Double
  lazy val half_hill_radius2: Double = half_hill_radius * half_hill_radius

  def air_free_altitude: Double

  println(s"$name -> $index")
  val currentState: MutableBodyState = initState.toMutableBodyState
  val ground_length_km: Int = (2 * math.Pi * radius / 1000).toInt
  val groundSpeedMsec: Double = currentState.ang_vel.toRad * radius
  val length: Double = 2 * math.Pi * radius

  val g: Double = G * mass / (radius * radius) // ускорение свободного падения, м/с^2

  var orbitRender: Option[OrbitRenderData] = None
}
