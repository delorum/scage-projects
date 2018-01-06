package com.github.dunnololda.scageprojects.orbitalkiller.planets

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{BodyState, MutableBodyState}
import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitData, _}

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
  val ground_length_km = (2 * math.Pi * radius / 1000).toInt
  val groundSpeedMsec = currentState.ang_vel.toRad * radius
  val length = 2 * math.Pi * radius

  val g = G * mass / (radius * radius) // ускорение свободного падения, м/с^2

  var orbitRender: Option[OrbitData] = None
}

