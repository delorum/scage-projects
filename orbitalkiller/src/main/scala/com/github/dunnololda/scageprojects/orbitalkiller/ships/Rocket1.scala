package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.PolygonShape
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.BodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ships.docking.DockingPoints
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ships.engines.Engine

abstract class Rocket1(
    index: Int,
    init_coord: DVec,
    init_velocity: DVec = DVec.dzero,
    init_rotation: Double = 0.0,
    ship_designer: Boolean)
  extends PolygonShip(
    index,
    "Доброта",
    init_coord,
    init_velocity,
    init_rotation,
    ship_designer,
    create_interface = false
  ) {
  private val _payload: Double = 196
  private var _fuel_mass: Double = 4

  // количество топлива для 1 секунды работы двигателя (расход топлива 4 кг/сек)
  def mass: Double = _payload + _fuel_mass

  override def fuelMass: Double = _fuel_mass

  override def fuelMass_=(m: Double): Unit = {
    _fuel_mass = m
  }

  val is_manned = false

  lazy val engine_size: Double = 0.5 * 0.1

  private val start_tact: Long = if (!ship_designer) system_evolution.tacts else 0
  private val work_tacts: Int = 1200 // количество тактов, за которое ракета пролетит 10 км

  lazy val points: List[DVec] = List(
    DVec(1.0, -20.0),
    DVec(2.0, -21.0),
    DVec(2.0, -18.0),
    DVec(1.0, -17.0),
    DVec(1.0, 18.0),
    DVec(0.0, 21.0),
    DVec(-1.0, 18.0),
    DVec(-1.0, -17.0),
    DVec(-2.0, -18.0),
    DVec(-2.0, -21.0),
    DVec(-1.0, -20.0)
  ).map(_ * 0.1)

  lazy val convex_parts: List[PolygonShape] = List(
    PolygonShape(List(DVec(-0.2, -2.0), DVec(-0.2, -2.1000001), DVec(-0.1, -2.0)), Nil),
    PolygonShape(List(DVec(-0.2, -2.0), DVec(0.1, -2.0), DVec(0.1, -1.8000001), DVec(-0.2, -1.8000001)), Nil),
    PolygonShape(List(DVec(0.1, -2.0), DVec(0.2, -2.1000001), DVec(0.2, -2.0)), Nil),
    PolygonShape(List(DVec(0.1, -2.0), DVec(0.2, -2.0), DVec(0.2, -1.8000001), DVec(0.1, -1.8000001)), Nil),
    PolygonShape(List(DVec(0.1, -1.7), DVec(0.1, -1.8000001), DVec(0.2, -1.8000001)), Nil),
    PolygonShape(List(DVec(-0.2, -1.8000001), DVec(-0.1, -1.8000001), DVec(-0.1, -1.7)), Nil),
    PolygonShape(List(DVec(-0.1, -1.8000001), DVec(0.1, -1.8000001), DVec(0.1, 1.8000001), DVec(-0.1, 1.8000001)), Nil),
    PolygonShape(List(DVec(-0.1, 1.8000001), DVec(0.1, 1.8000001), DVec(0.0, 2.1000001)), Nil)
  )

  val wreck_parts: List[PolygonShape] = List(
    PolygonShape(
      List(DVec(-0.2, -2.1000001), DVec(-0.1, -2.0), DVec(0.1, -1.9), DVec(-0.2, -1.8000001)),
      List( // 1 NOT CONVEX
        PolygonShape(List(DVec(-0.2, -1.8000001), DVec(-0.1, -2.0), DVec(0.1, -1.9)), List()),
        PolygonShape(List(DVec(-0.2, -1.8000001), DVec(-0.2, -2.1000001), DVec(-0.1, -2.0)), List())
      )
    ),
    PolygonShape(
      List(DVec(-0.1, -2.0), DVec(0.1, -2.0), DVec(0.2, -1.9), DVec(0.1, -1.7), DVec(0.1, -1.9)),
      List()
    ), // 2
    PolygonShape(List(DVec(0.1, -2.0), DVec(0.2, -2.1000001), DVec(0.2, -1.9)), List()), // 3
    PolygonShape(List(DVec(0.1, -1.7), DVec(0.2, -1.9), DVec(0.2, -1.8000001)), List()), // 4
    PolygonShape(List(DVec(-0.2, -1.8000001), DVec(0.1, -1.9), DVec(0.0, -1.7), DVec(-0.1, -1.7)), List()), // 5
    PolygonShape(List(DVec(0.1, -1.3000001), DVec(0.0, -1.4), DVec(0.0, -1.7), DVec(0.1, -1.9)), List()), // 6
    PolygonShape(
      List(
        DVec(-0.1, -1.7),
        DVec(0.0, -1.7),
        DVec(0.0, -1.4),
        DVec(0.1, -1.3000001),
        DVec(0.1, -1.1),
        DVec(-0.1, -1.3000001)
      ),
      List( // 7 NOT CONVEX
        PolygonShape(List(DVec(-0.1, -1.7), DVec(0.0, -1.7), DVec(0.0, -1.4), DVec(-0.1, -1.3000001)), List()),
        PolygonShape(List(DVec(-0.1, -1.3000001), DVec(0.0, -1.4), DVec(0.1, -1.3000001), DVec(0.1, -1.1)), List())
      )
    ),
    PolygonShape(List(DVec(-0.1, -1.3000001), DVec(0.1, -1.1), DVec(0.1, -0.90000004), DVec(-0.1, -0.8)), List()), // 8
    PolygonShape(List(DVec(-0.1, -0.8), DVec(0.1, -0.90000004), DVec(0.1, -0.7), DVec(-0.1, -0.7)), List()), // 9
    PolygonShape(List(DVec(-0.1, -0.7), DVec(0.1, -0.5), DVec(-0.1, -0.3)), List()), // 10
    PolygonShape(List(DVec(-0.1, -0.7), DVec(0.1, -0.7), DVec(0.1, -0.5)), List()), // 11
    PolygonShape(List(DVec(-0.1, -0.3), DVec(0.1, -0.5), DVec(0.1, -0.2)), List()), // 12
    PolygonShape(List(DVec(-0.1, -0.3), DVec(0.1, -0.2), DVec(0.1, -0.1), DVec(-0.1, 0.0)), List()), // 13
    PolygonShape(List(DVec(-0.1, 0.0), DVec(0.1, -0.1), DVec(0.0, 0.3)), List()), // 14
    PolygonShape(List(DVec(-0.1, 0.0), DVec(0.0, 0.3), DVec(0.0, 0.6), DVec(-0.1, 0.4)), List()), // 15
    PolygonShape(List(DVec(0.0, 0.3), DVec(0.1, -0.1), DVec(0.1, 0.5), DVec(0.0, 0.5)), List()), // 16
    PolygonShape(
      List(DVec(-0.1, 0.4), DVec(0.0, 0.6), DVec(0.0, 0.5), DVec(0.1, 0.5), DVec(0.1, 0.7), DVec(-0.1, 0.8)),
      List( // 17 NOT CONVEX
        PolygonShape(List(DVec(-0.1, 0.8), DVec(-0.1, 0.4), DVec(0.0, 0.6)), List()),
        PolygonShape(List(DVec(-0.1, 0.8), DVec(0.0, 0.6), DVec(0.1, 0.6), DVec(0.1, 0.7)), List()),
        PolygonShape(List(DVec(0.0, 0.6), DVec(0.0, 0.5), DVec(0.1, 0.5), DVec(0.1, 0.6)), List())
      )
    ),
    PolygonShape(List(DVec(-0.1, 0.8), DVec(0.1, 0.7), DVec(0.1, 1.0), DVec(-0.1, 1.1)), List()), // 18
    PolygonShape(List(DVec(-0.1, 1.1), DVec(0.1, 1.0), DVec(0.1, 1.2), DVec(-0.1, 1.3000001)), List()), // 19
    PolygonShape(List(DVec(-0.1, 1.3000001), DVec(0.1, 1.2), DVec(0.1, 1.5), DVec(0.0, 1.5)), List()), // 20
    PolygonShape(List(DVec(-0.1, 1.3000001), DVec(0.0, 1.5), DVec(-0.1, 1.6)), List()), // 21
    PolygonShape(List(DVec(-0.1, 1.6), DVec(0.0, 1.5), DVec(0.1, 1.5), DVec(0.1, 1.7)), List()), // 22
    PolygonShape(List(DVec(-0.1, 1.6), DVec(0.1, 1.7), DVec(-0.1, 1.7)), List()), // 23
    PolygonShape(List(DVec(-0.1, 1.8000001), DVec(0.0, 1.7), DVec(0.0, 2.1000001)), List()), // 24
    PolygonShape(List(DVec(-0.1, 1.8000001), DVec(-0.1, 1.7), DVec(0.0, 1.7)), List()), // 25
    PolygonShape(List(DVec(0.0, 1.7), DVec(0.1, 1.7), DVec(0.1, 1.8000001), DVec(0.0, 2.1000001)), List()) // 26
  )

  val docking_points: List[DockingPoints] = Nil

  val two = new Engine(
    2,
    position = DVec(0.0, -20.0) * 0.1,
    force_dir = DVec(0.0, 1.0),
    max_power = 100000, // такая сила разгонит 200-килограммовую ракету до 500 м/сек за 1 секунду
    default_power_percent = 1,
    fuel_consumption_per_sec_at_full_power = 4,
    this
  )

  val engines: List[Engine] = List(two)

  val engines_by_keycodes: Map[Int, Engine] = Map(
    KEY_NUMPAD2 -> two
  )

  def preserveVelocity(vel: DVec): Unit = {}

  def preserveAngularVelocity(ang_vel_deg: Double): Unit = {}

  override def drawIfAliveAfterRotation(): Unit = {
    drawSlidingLines(actualDrawPoints, colorIfPlayerAliveOrRed(WHITE))
    engines.foreach(e =>
      drawEngine(e))
  }

  override def afterStep(time_msec: Long): Unit = {
    super.afterStep(time_msec)
    if (system_evolution.tacts - start_tact > work_tacts) {
      currentState.vel = init_velocity
      kill("Ракета самоуничтожилась", crash = true)
    }
  }

  override lazy val initState: BodyState = BodyState(
    index,
    mass,
    acc = DVec.zero,
    vel = init_velocity,
    coord = init_coord,
    ang_acc = 0,
    ang_vel = 0,
    ang = init_rotation,
    shape = PolygonShape(points, convex_parts),
    is_static = false,
    is_bullet = true
  )

  override def checkCriticalCollision(): Unit = {
    super.checkCriticalCollision()
    currentState.contacts.foreach(c => {
      val obstacle = if (c.a.index != index) c.a else c.b
      shipsHolder
        .shipByIndex(obstacle.index)
        .foreach(s => {
          if (!s.isCrashed) {
            s.kill("Корабль уничтожен ракетным ударом", crash = true)
          }
        })
    })
  }
}
