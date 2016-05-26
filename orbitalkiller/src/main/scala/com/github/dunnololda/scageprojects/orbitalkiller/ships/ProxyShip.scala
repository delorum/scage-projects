package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class ProxyShip(ship1:PolygonShip,
                ship2:PolygonShip,
                ship1_hull_points:List[DVec],
                ship2_hull_points:List[DVec],
                ship1_dp:DockingPoints,
                ship2_dp:DockingPoints) extends PolygonShip(
  ScageId.nextId,
  s"${ship1.name}-${ship2.name}",
  (ship1.mass*ship1.coord + ship2.mass*ship2.coord)/(ship1.mass + ship2.mass),
  (ship1.mass*ship1.linearVelocity + ship2.mass*ship2.linearVelocity)/(ship1.mass + ship2.mass),
  ship1.rotation, false, false) {

  lazy val ship1_coord_diff = ship1.coord - init_coord
  lazy val ship1_draw_points = ship1.draw_points.map(_ + ship1_coord_diff)
  lazy val ship2_coord_diff = ship2.coord - init_coord
  lazy val ship2_draw_points = ship2.draw_points.map(_ + ship2_coord_diff)
  lazy val ship2_rotation_diff = ship2.rotation - ship1.rotation

  val is_player = ship1.index == player_ship.index || ship2.index == player_ship.index
  
  def updateShipState(ship_index:Int): Unit = {
    if(ship_index == ship1.index) {
      ship1.currentState.coord = currentState.coord + ship1_coord_diff.rotateDeg(rotation)
      ship1.currentState.vel = currentState.vel
      ship1.currentState.ang = rotation
      ship1.currentState.ang_vel = currentState.ang_vel
    } else if(ship_index == ship2.index) {
      ship2.currentState.coord = currentState.coord + ship2_coord_diff.rotateDeg(rotation)
      ship2.currentState.vel = currentState.vel
      ship2.currentState.ang = rotation  + ship2_rotation_diff
      ship2.currentState.ang_vel = currentState.ang_vel
    }
  }

  def mass = ship1.mass + ship2.mass

  override val engines: List[Engine] = ship1.engines ::: ship2.engines

  override def fuelMass: Double = ship1.fuelMass + ship2.fuelMass

  override def fuelMass_=(m: Double): Unit = {}

  override lazy val convex_parts: List[PolygonShape] = {
    ship1.convex_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.convex_parts.map(p => p.copy(points = p.points.map(_ + ship2_coord_diff)))
  }

  /**
   * Точки, обозначающие корпус корабля. Задаются координатами относительно центра масс. Фактическую координату точки в данный момент времени
   * можно получить, повернув на текущий угол и прибавив координату ц.м.
   * @return
   */
  override lazy val points: List[DVec] = ship1_hull_points.map(_ + ship1_coord_diff) ::: ship2_hull_points.map(_ + ship2_coord_diff)

  override val wreck_parts: List[PolygonShape] = {
    ship1.wreck_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.wreck_parts.map(p => p.copy(points = p.points.map(_ + ship2_coord_diff)))
  }

  override lazy val engine_size: Double = math.min(ship1.engine_size, ship2.engine_size)

  override def preserveAngularVelocity(ang_vel_deg: Double): Unit = {}

  override def preserveVelocity(vel: DVec): Unit = {}

  override val is_manned: Boolean = ship1.is_manned || ship2.is_manned

  override val docking_points: List[DockingPoints] = {
    ship1.docking_points.filterNot(dp => dp.index == ship1_dp.index).map(dp => {
      new DockingPoints(
        dp.p1 + ship1_coord_diff,
        dp.p2 + ship1_coord_diff,
        this,
        dp.disabled_engine,
        dp.part_of_shape.map(_ + ship1_coord_diff))
    }) :::
    ship2.docking_points.filterNot(dp => dp.index == ship2_dp.index).map(dp => {
      new DockingPoints(
        dp.p1 + ship2_coord_diff,
        dp.p2 + ship2_coord_diff,
        this,
        dp.disabled_engine.map(_ + 10),
        dp.part_of_shape.map(_ + ship2_coord_diff))
    })
  }

  override val engines_by_keycodes_map: Map[Int, Engine] = Map()

  override def consumeFuel() {
    ship1.consumeFuel()
    ship2.consumeFuel()
  }

  override def currentReactiveForce(time: Long, bs: BodyState): DVec = {
    ship1.currentReactiveForce(time, bs) + ship2.currentReactiveForce(time, bs)
  }

  override def currentReactiveForce(tacts: Long, bs: MutableBodyState): DVec = {
    ship1.currentReactiveForce(tacts, bs) + ship2.currentReactiveForce(tacts, bs)
  }

  override def currentTorque(time: Long, coord_diff:DVec = DVec.zero): Double = {
    ship1.currentTorque(time, coord_diff + ship1_coord_diff) + ship2.currentTorque(time, coord_diff + ship2_coord_diff)
  }

  override def kill(reason: String, crash: Boolean): Unit = {
    ship1.kill(reason, crash)
    ship2.kill(reason, crash)
  }

  /**
   * Сколько тактов работы двигателя потребуется, чтобы достичь скорости to, при условии, что текущая скорость равна from,
   * ускорение равно a, один такт равен dt секунд
   *
   * @param to - скорость, которой хотим достичь
   * @param from - текущая скорость
   * @param a - ускорение
   * @param dt - сколько секунд в одном такте
   * @return два значения: сколько тактов потребуется, какой значения скорости фактически достигнем
   */
  private def howManyTacts(to: Double, from: Double, a: Double, dt: Double): (Int, Double) = {
    val tacts = ((to - from) / (a * dt)).toInt + 1
    val result_to = from + tacts * a * dt
    (tacts, result_to)
  }

  private val default_percent_seq = ((99.0 to 1.0 by -1.0) ++ (0.9 to 0.1 by -0.1)).view
  def maxPossiblePowerAndTactsForRotation(e:Engine,
                                          need_ang_vel: Double): (Int, Double) = {
    val coord_diff = e.ship.index match {
      case ship1.index => ship1_coord_diff
      case ship2.index => ship2_coord_diff
      case _ => DVec.zero
    }
    default_percent_seq.map {
      case percent =>
        val power = e.max_power * 0.01 * percent
        val torque = (-e.force_dir * power) */ (e.position + coord_diff)
        val ang_acc = (torque / currentState.I).toDeg
        (howManyTacts(need_ang_vel, currentState.ang_vel, ang_acc, base_dt), power, percent)
    }.find {
      case ((tacts, result_ang_vel), power, percent) =>
        //println(s"maxPossiblePowerAndTactsForRotation find: $power, $percent: ${math.abs(need_ang_vel - result_ang_vel)}")
        val check = math.abs(need_ang_vel - result_ang_vel) < angular_velocity_error
        /*if(check) {
          println(s"maxPossiblePowerAndTactsForRotation = ($tacts, $power, $percent)")
        }*/
        check
    }.map {
      case ((tacts, result_to), power, percent) =>
        (tacts, power)
    }.getOrElse({
      //println("maxPossiblePowerForRotation fallback")
      (correction_check_period, e.max_power * 0.1)
    })
  }

  override def drawShip(): Unit = {
    if (dockData.isEmpty && !drawMapMode && (is_player || coord.dist2(player_ship.coord) < 100000 * 100000)) {
      if (isAlive) {
        openglLocalTransform {
          openglMove(coord - base)
          drawFilledCircle(DVec.zero, 0.3, colorIfPlayerAliveOrRed(GREEN)) // mass center

          if (OrbitalKiller.globalScale >= 0.8) {
            if (is_player) {
              if (!InterfaceHolder.linearVelocityInfo.isMinimized) {
                // current velocity
                drawArrow(DVec.zero, linearVelocity.n * radius, colorIfPlayerAliveOrRed(BLUE))
                drawArrow(DVec.zero, relativeLinearVelocity.n * radius, colorIfPlayerAliveOrRed(InterfaceHolder.linearVelocityInfo.color))
              }
              if (!InterfaceHolder.sunRelativeInfo.isMinimized) {
                // direction to earth
                drawArrow(Vec.zero, (sun.coord - coord).n * radius, colorIfPlayerAliveOrRed(InterfaceHolder.sunRelativeInfo.color))
              }
              if (!InterfaceHolder.earthRelativeInfo.isMinimized) {
                // direction to earth
                drawArrow(Vec.zero, (earth.coord - coord).n * radius, colorIfPlayerAliveOrRed(InterfaceHolder.earthRelativeInfo.color))
              }
              if (!InterfaceHolder.moonRelativeInfo.isMinimized) {
                // direction to moon
                drawArrow(Vec.zero, (moon.coord - coord).n * radius, colorIfPlayerAliveOrRed(InterfaceHolder.moonRelativeInfo.color))
              }
              InterfaceHolder.shipInterfaces.foreach(si => {
                if (!si.isMinimized && si.monitoring_ship.isAlive) {
                  drawArrow(Vec.zero, (si.monitoring_ship.coord - coord).n * radius, colorIfPlayerAliveOrRed(si.color))
                }
              })
            }
          }

          openglLocalTransform {
            openglRotateDeg(rotation)

            ship1.engines.foreach {
              case e => ship1.drawEngine(e, ship1_coord_diff)
            }
            ship2.engines.foreach {
              case e => ship2.drawEngine(e, ship2_coord_diff)
            }

            if (OrbitalKiller.globalScale >= 0.8) {
              if(is_player) {
                if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
                  docking_points.foreach(dp => {
                    drawFilledCircle(dp.p1, 0.3, colorIfPlayerAliveOrRed(RED))
                    drawFilledCircle(dp.p2, 0.3, colorIfPlayerAliveOrRed(RED))
                  })
                }
              } else {
                if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
                  docking_points.foreach(dp => {
                    val (p1_on_the_right_way, p2_on_the_right_way) = OrbitalKiller.player_ship.nearestDockingPoints(coord).map(_.pointsOnTheRightWay(dp)).getOrElse((false, false))

                    val c1 = if (p1_on_the_right_way) GREEN else RED
                    val c2 = if (p2_on_the_right_way) GREEN else RED

                    val v1 = (dp.p1 - dp.p2).n
                    val v2 = v1.perpendicular

                    drawDashedLine(dp.p1, dp.p1 + v2 * 100, 2.5, colorIfPlayerAliveOrRed(c1))
                    drawDashedLine(dp.p2, dp.p2 + v2 * 100, 2.5, colorIfPlayerAliveOrRed(c2))

                    drawFilledCircle(dp.p1, 0.3, colorIfPlayerAliveOrRed(RED))
                    drawCircle(dp.p1, dp.dock_dist, colorIfPlayerAliveOrRed(RED))
                    drawFilledCircle(dp.p2, 0.3, colorIfPlayerAliveOrRed(RED))
                    drawCircle(dp.p2, dp.dock_dist, colorIfPlayerAliveOrRed(RED))
                  })
                }
              }
            }

            openglMove(ship1_coord_diff)
            drawSlidingLines(ship1.draw_points, WHITE)
            if (OrbitalKiller.globalScale >= 0.8) {
              drawFilledCircle(ship1_dp.p1, 0.3, colorIfPlayerAliveOrRed(GREEN))
              drawFilledCircle(ship1_dp.p2, 0.3, colorIfPlayerAliveOrRed(GREEN))
            }
          }

          openglLocalTransform {
            openglRotateDeg(rotation  + ship2_rotation_diff)
            openglMove(ship2_coord_diff)
            drawSlidingLines(ship2.draw_points, WHITE)
          }
        }
      }
    }
  }
}
