package com.github.dunnololda.scageprojects.orbitalkiller.planets

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.{BodyState, CircleShape}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.MathUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.PhysicsUtils._

import scala.collection.Seq

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
