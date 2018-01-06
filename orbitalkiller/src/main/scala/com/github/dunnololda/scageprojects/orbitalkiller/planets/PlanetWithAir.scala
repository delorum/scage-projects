package com.github.dunnololda.scageprojects.orbitalkiller.planets

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.MutableBodyState

class PlanetWithAir(index: Int,
                    name: String,
                    mass: Double,
                    init_coord: DVec,
                    init_velocity: DVec,
                    init_ang_vel: Double,
                    radius: Double,
                    orbiting_body: CelestialBody,
                    air_free_altitude: Double,
                    val T0: Int, // temperature at sea level, K
                    val L: Double, // temperature lapse rate, K/m
                    val P0: Double, // pressure at sea level, N/m^2
                    val M: Double, // molar mass or air, kg/mole
                    val R: Double) // ideal gas constant, Joules/(K*mole)
  extends Planet(index, name, mass, init_coord, init_velocity, init_ang_vel, radius, orbiting_body, air_free_altitude) {
  def altitude(ship_coord: DVec, planet_coord: DVec): Double = ship_coord.dist(planet_coord) - radius

  def velocityRelativeToAir(ship_coord: DVec, ship_velocity: DVec, planet_coord: DVec, planet_velocity: DVec, planet_ang_vel: Double): DVec = {
    (ship_velocity - planet_velocity) - (ship_coord - planet_coord).p * (planet_ang_vel.toRad * ship_coord.dist(planet_coord))
  }

  def temperature(h: Double) = {
    T0 - L * h
  }

  def airPressurePascale(h: Double): Double = {
    val T = temperature(h)
    if (T <= 0) 0
    else {
      P0 * math.pow(T / T0, g * M / (R * L))
    }
  }

  def airPressureMmHg(h: Double): Double = {
    airPressurePascale(h) / 133.3
  }

  // A - Reference area (of the front of the ship)
  // C - Drag coefficient
  private def airResistance(v: DVec, h: Double, A: Double, C: Double /*, check_obscure_func: DVec => Boolean*/): DVec = {
    val T = temperature(h)
    if (T <= 0) DVec.zero
    else {
      /*if (check_obscure_func(v.n)) DVec.zero
      else {*/
      val P = airPressurePascale(h)
      val ro = P * M / (R * T) // density of air
      val F = 0.5 * ro * A * C * v.norma2
      -v.n * F
      /*}*/
    }
  }

  private def airResistance(ship_coord: DVec,
                            ship_velocity: DVec,
                            planet_coord: DVec,
                            planet_velocity: DVec,
                            planet_ang_vel: Double,
                            A: Double,
                            C: Double /*,
                            check_obscure_func: DVec => Boolean*/): DVec = {
    airResistance(velocityRelativeToAir(ship_coord, ship_velocity, planet_coord, planet_velocity, planet_ang_vel),
      altitude(ship_coord, planet_coord),
      A,
      C /*,
      check_obscure_func*/)
  }

  def airResistance(ship_state: MutableBodyState,
                    planet_state: MutableBodyState,
                    /*other_ship_states: Seq[MutableBodyState],*/
                    A: Double,
                    C: Double): DVec = {
    /*def _checkIfObscuredByAnotherShip(ship_dir_relative_to_air: DVec, other_ship_states_left: Seq[MutableBodyState] = other_ship_states): Boolean = {
      if (other_ship_states_left.isEmpty) false
      else {
        val other_ship_state = other_ship_states_left.head
        (ship_state.polygonShape, other_ship_state.polygonShape) match {
          case (Some(our_shape), Some(other_shape)) =>
            val d2 = (our_shape.radius + other_shape.radius) * (our_shape.radius + other_shape.radius)
            (ship_state.coord.dist2(other_ship_state.coord) <= d2) && {
              val res = (other_shape.points ::: List(other_shape.points.head)).sliding(2).exists {
                case List(p1, p2) => areLinesIntersect(
                  ship_state.coord,
                  ship_state.coord + ship_dir_relative_to_air * (our_shape.radius + other_shape.radius),
                  other_ship_state.coord + p1.rotateDeg(other_ship_state.ang), other_ship_state.coord + p2.rotateDeg(other_ship_state.ang))
              }
              /*if(ship_state.index == player_ship.index) {
                if (res) {
                  println(s"${nameByIndex(other_ship_state.index).get} obscuring our ship ${nameByIndex(ship_state.index).get}")
                } else {
                  println(s"${nameByIndex(other_ship_state.index).get} is not obscuring our ship ${nameByIndex(ship_state.index).get} but close to us")
                }
              }*/
              res
            } || _checkIfObscuredByAnotherShip(ship_dir_relative_to_air, other_ship_states_left.tail)
          case _ => _checkIfObscuredByAnotherShip(ship_dir_relative_to_air, other_ship_states_left.tail)
        }
      }
    }*/
    //val f = (ship_dir_relative_to_air: DVec) => _checkIfObscuredByAnotherShip(ship_dir_relative_to_air)
    airResistance(ship_state.coord, ship_state.vel, planet_state.coord, planet_state.vel, planet_state.ang_vel, A, C /*, f*/)
  }

  def airPressureMmHg(ship_coord: DVec, planet_coord: DVec): Double = {
    airPressureMmHg(altitude(ship_coord, planet_coord))
  }

  def terminalVelocity(ship_mass: Double, ship_coord: DVec, planet_coord: DVec, A: Double, C: Double): Option[Double] = {
    val h = altitude(ship_coord, planet_coord)
    val P = airPressurePascale(h)
    val T = temperature(h)
    if (T <= 0) None
    else {
      val ro = P * M / (R * T) // density of air
      Some(math.sqrt(2 * ship_mass * g / (ro * A * C)))
    }
  }

  // another model
  // http://fiz.1september.ru/articlef.php?ID=200801702

  /*def airResistance(v:DVec, h:Double):DVec = {
    val c = 0.045                   // безразмерный коэффициент (равный 0,045 для «каплевидного» тела)
    val ro0 = 1.22                  // плотность воздуха на поверхности Земли, кг/м^3
    val beta = 5.6E-5               // м^-1
    val S = math.Pi*3*3             // площадь поперечного сечения тела
    val ro = ro0*math.exp(-beta*h)  // плотность воздуха на высоте h
    val k = c*ro*S                  // коэффициент лобового сопротивления
    val F = k*v.norma2
    //val F = 0.045*v.norma2*1.225*math.exp(-5.6E-5*h)*math.Pi*3*3
    //val F = 0.045*v.norma2*1.225*math.exp(-1.4E-4*h)*math.Pi*3*3
    -v.n*F
  }*/

  /*def airPressurePascale(h:Double):Double = {
    101325*math.exp(-1.1855831477936685E-4*h)
  }*/
}
