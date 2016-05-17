package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

import scala.collection.Seq

trait CelestialBody {
  def index: Int

  def name: String

  def coord: DVec

  def linearVelocity: DVec

  def mass: Double

  def radius: Double

  def initState: BodyState

  def half_hill_radius: Double

  def air_free_altitude: Double

  println(s"$name -> $index")
  val currentState: MutableBodyState = initState.toMutableBodyState
  val ground_length_km = (2 * math.Pi * radius / 1000).toInt
  val groundSpeedMsec = currentState.ang_vel.toRad * radius
  val length = 2 * math.Pi * radius

  val g = G * mass / (radius * radius) // ускорение свободного падения, м/с^2

  var orbitRender: Option[OrbitData] = None
}

class Planet(val index: Int,
             val name: String,
             val mass: Double,
             val init_coord: DVec,
             val init_velocity: DVec,
             val init_ang_vel: Double,
             val radius: Double,
             val orbiting_body: CelestialBody,
             val air_free_altitude: Double) extends CelestialBody {

  def coord = currentState.coord

  def linearVelocity = currentState.vel

  def initState: BodyState = BodyState(
    index,
    mass,
    acc = DVec.dzero,
    vel = init_velocity,
    coord = init_coord,
    ang_acc = 0,
    ang_vel = init_ang_vel,
    ang = 0,
    shape = CircleShape(radius),
    is_static = false)

  val half_hill_radius = halfHillRadius(mass, coord.dist(orbiting_body.coord), orbiting_body.mass)

  // рельеф планеты: треугольные горы. два параметра: высота в метрах, ширина основания в метрах
  private val ground_features = Array.ofDim[(Int, Int)](ground_length_km)
  (0 until ground_length_km).foreach(x => ground_features(x) = (100 + (math.random * 900).toInt, 50 + (math.random * 300).toInt))

  private def groundFeatureNear(point_km: Double): (Int, Int) = {
    if (point_km < 0) groundFeatureNear(point_km + ground_length_km)
    else if (point_km >= ground_length_km) groundFeatureNear(point_km - ground_length_km)
    else ground_features(point_km.toInt)
  }

  private var data_initialized = false
  private val half_render_length_km = 50
  private val alpha = half_render_length_km * 2.0 * 1000 * 180 / math.Pi / radius
  private var viewpoint_dist: Double = _
  private var to_viewpoint: DVec = _
  private var points: Seq[DVec] = _
  private var ground_position_ang: Double = _
  private var ground_position_km: Double = _
  private var ground_features_near: Seq[(DVec, Int, Int)] = _

  private def updateRenderData() {
    viewpoint_dist = math.abs((player_ship.coord + shipOffset).dist(coord) - radius)
    if (viewpoint_dist < 50000) {
      //val before_to_viewpoint = (ship.coord + shipOffset - coord).n*(radius - 1000)
      to_viewpoint = (player_ship.coord + shipOffset - coord).n * radius
      points = for {
        ang <- -alpha to alpha by 0.01
        point = to_viewpoint.rotateDeg(ang)
      } yield point
      ground_position_ang = correctAngle(DVec(0, 1).deg360(to_viewpoint) - currentState.ang)
      ground_position_km = {
        val x = ground_position_ang / 360.0 * 2 * math.Pi * radius / 1000
        if (x - half_render_length_km < 0) x + ground_length_km else x
      }
      ground_features_near = for {
        real_point <- ground_position_km - half_render_length_km to ground_position_km + half_render_length_km - 1 by 1.0
        (w, h) = groundFeatureNear(real_point)
        point_ang = (360.0 * real_point.toInt / ground_length_km) - ground_position_ang
        p = to_viewpoint.rotateDeg(point_ang)
      } yield (p, w, h)
    }
  }

  action(1) {
    updateRenderData()
  }

  actionIgnorePause(1) {
    updateRenderData()
    data_initialized = true
    deleteSelf()
  }

  render {
    if (data_initialized && /*renderingEnabled &&*/ !drawMapMode) {
      if (viewpoint_dist < 500) {
        openglLocalTransform {
          openglMove(coord - base)

          ground_features_near.foreach { case (p, w, h) =>
            drawLine(p + p.p * w / 2, p + p.n * h, WHITE)
            drawLine(p - p.p * w / 2, p + p.n * h, WHITE)
            drawLine(p, p + p.n * h, WHITE)
          }
        }
        openglLocalTransform {
          openglMove(player_ship.coord - base)
          val pa = (coord - player_ship.coord).n * (player_ship.coord.dist(coord) - radius) + (coord - player_ship.coord).p * 70000
          val pb = (coord - player_ship.coord).n * (player_ship.coord.dist(coord) - radius) + (coord - player_ship.coord).p * (-70000)
          drawLine(pa, pb, WHITE)
        }
      } else if (viewpoint_dist < 50000) {
        openglLocalTransform {
          openglMove(coord - base)
          drawSlidingLines(points, WHITE)
          ground_features_near.foreach { case (p, w, h) =>
            drawLine(p + p.p * w / 2, p + p.n * h, WHITE)
            drawLine(p - p.p * w / 2, p + p.n * h, WHITE)
            drawLine(p, p + p.n * h, WHITE)
          }
        }
      }
    }
  }
}

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
  private def airResistance(v: DVec, h: Double, A: Double, C: Double, check_obscure_func: DVec => Boolean): DVec = {
    val T = temperature(h)
    if (T <= 0) DVec.zero
    else {
      if (check_obscure_func(v.n)) DVec.zero
      else {
        val P = airPressurePascale(h)
        val ro = P * M / (R * T) // density of air
        val F = 0.5 * ro * A * C * v.norma2
        -v.n * F
      }
    }
  }

  private def airResistance(ship_coord: DVec,
                            ship_velocity: DVec,
                            planet_coord: DVec,
                            planet_velocity: DVec,
                            planet_ang_vel: Double,
                            A: Double,
                            C: Double,
                            check_obscure_func: DVec => Boolean): DVec = {
    airResistance(velocityRelativeToAir(ship_coord, ship_velocity, planet_coord, planet_velocity, planet_ang_vel),
      altitude(ship_coord, planet_coord),
      A,
      C,
      check_obscure_func)
  }

  def airResistance(ship_state: MutableBodyState,
                    planet_state: MutableBodyState,
                    other_ship_states: Seq[MutableBodyState],
                    A: Double,
                    C: Double): DVec = {
    def _checkIfObscuredByAnotherShip(ship_dir_relative_to_air: DVec, other_ship_states_left: Seq[MutableBodyState] = other_ship_states): Boolean = {
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
    }
    val f = (ship_dir_relative_to_air: DVec) => _checkIfObscuredByAnotherShip(ship_dir_relative_to_air)
    airResistance(ship_state.coord, ship_state.vel, planet_state.coord, planet_state.vel, planet_state.ang_vel, A, C, f)
  }

  def airPressureMmHg(ship_coord: DVec, planet_coord: DVec): Double = {
    airPressureMmHg(altitude(ship_coord, planet_coord))
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

class Star(val index: Int,
           val name: String,
           val mass: Double,
           val coord: DVec,
           /*val init_ang_vel:Double,*/
           val radius: Double) extends CelestialBody {
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
    is_static = true)

  def linearVelocity: DVec = DVec.dzero

  val half_hill_radius: Double = 0.0
  val air_free_altitude: Double = 0.0
}

