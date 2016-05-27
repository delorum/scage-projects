package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class ProxyShip(ship1:PolygonShip,
                ship1_init_coord:DVec,
                ship1_init_rotation:Double,
                ship1_dp:DockingPoints,
                ship2:PolygonShip,
                ship2_init_coord:DVec,
                ship2_init_rotation:Double,
                ship2_dp:DockingPoints) extends PolygonShip(
  ScageId.nextId,
  s"${ship1.name}-${ship2.name}",
  (ship1.mass*ship1.coord + ship2.mass*ship2.coord)/(ship1.mass + ship2.mass),
  (ship1.mass*ship1.linearVelocity + ship2.mass*ship2.linearVelocity)/(ship1.mass + ship2.mass),
  ship1.rotation, false, false) {

  println(s"init_rotation=$init_rotation")
  println(s"ship1_init_coord=$ship1_init_coord")
  println(s"ship2_init_coord=$ship2_init_coord")
  println(s"init_coord=$init_coord")

  lazy val ship1_coord_diff = (ship1_init_coord - init_coord).rotateDeg(-init_rotation)
  lazy val ship2_rotation_diff = ship2_init_rotation - ship1_init_rotation
  lazy val ship2_coord_diff = (ship2_init_coord - init_coord).rotateDeg(-init_rotation)
  lazy val ship2_draw_points = ship2.draw_points.map(_.rotateDeg(ship2_rotation_diff))

  def coordDiff(ship_index:Int):DVec = {
    ship_index match {
      case ship1.index => ship1_coord_diff
      case ship2.index => ship2_coord_diff
      case _ => DVec.zero
    }
  }

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
    ship2.convex_parts.map(p => p.copy(points = p.points.map(p => p.rotateDeg(ship2_rotation_diff) + ship2_coord_diff)))
  }

  /**
   * Все другие корабли, отсортированные по расстоянию по убыванию (первый - ближайший).
   * @return
   */
  override def shipsNear: Seq[PolygonShip] = ShipsHolder.ships.filter(s => {
    s.currentState.active &&
    s.index != index &&
    s.index != ship1.index && s.index != ship2.index &&
    !dock_data.exists(dd => s.index != dd.dock_to_ship.index && s.index != dd.proxy_ship.index) &&
    s.isAlive
  }).sortBy(s => coord.dist2(s.coord))

  /**
   * Корабли ближе x км от нас. Метод используется для вычисления автоматического наведения ракет.
   * @param x - расстояние в километрах
   * @return
   */
  override def shipsCloserXKm(x: Long): Seq[PolygonShip] = ShipsHolder.ships.filter(s => {
    s.currentState.active &&
    s.index != index &&
    s.index != ship1.index && s.index != ship2.index &&
    !dock_data.exists(dd => s.index != dd.dock_to_ship.index && s.index != dd.proxy_ship.index) &&
    s.isAlive &&
    s.coord.dist2(coord) < x * 1000l * x * 1000l
  }).sortBy(s => coord.dist2(s.coord))

  /**
   * Корабль ближе 500 км от нас, интерфейс которого не свернут. Если таких несколько, то ближайший.
   * Метод используется в алогритмах автоматического уравнивания скорости, поддержания направления, стыковки
   * @return
   */
  override def shipCloser500KmNonMinimized: Option[PolygonShip] = {
    ShipsHolder.ships.filter(s => {
      s.currentState.active &&
      s.index != index &&
      s.index != ship1.index && s.index != ship2.index &&
      !dock_data.exists(dd => s.index != dd.dock_to_ship.index && s.index != dd.proxy_ship.index) &&
      s.isAlive &&
      s.coord.dist2(coord) < 500 * 1000l * 500 * 1000l &&
      s.shipInterface.exists(!_.isMinimized)
    }).sortBy(s => coord.dist2(s.coord)).headOption
  }

  override def tryDock:Boolean = ship1.tryDock || ship2.tryDock
  override def tryUndock:Boolean = ship1.tryUndock || ship2.tryUndock

  /**
   * Точки, обозначающие корпус корабля. Задаются координатами относительно центра масс. Фактическую координату точки в данный момент времени
   * можно получить, повернув на текущий угол и прибавив координату ц.м.
   * @return
   */
  override lazy val points: List[DVec] = {
    ship1_dp.ordered_hull.map(_ + ship1_coord_diff) :::
    ship2_dp.ordered_hull.map(p => p.rotateDeg(ship2_rotation_diff) + ship2_coord_diff)
  }

  override val wreck_parts: List[PolygonShape] = {
    ship1.wreck_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.wreck_parts.map(p => p.copy(points = p.points.map(p => p.rotateDeg(ship2_rotation_diff) + ship2_coord_diff)))
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
        dp.ordered_hull.map(_ + ship1_coord_diff))
    }) :::
    ship2.docking_points.filterNot(dp => dp.index == ship2_dp.index).map(dp => {
      new DockingPoints(
        dp.p1 + ship2_coord_diff,
        dp.p2 + ship2_coord_diff,
        this,
        dp.disabled_engine.map(_ + 10),
        dp.ordered_hull.map(_ + ship2_coord_diff))
    })
  }

  override val engines_by_keycodes: Map[Int, Engine] = Map()

  override def consumeFuel() {
    ship1.consumeFuel()
    ship2.consumeFuel()
  }

  override def checkDockingSituation(): Unit = {
    if(tryDock) {
      dock()
    }
    if(tryUndock) {
      if(dock_data.nonEmpty) {
        undock()
      } else {
        ship1.undock()
      }
    }
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
            //drawSlidingLines(draw_points, GREEN)
            convex_parts.foreach(p => drawSlidingLines(p.points ::: List(p.points.head), GREEN))
            //wreck_parts.foreach(p => drawSlidingLines(p.points ::: List(p.points.head), GREEN))
            ship1.engines.foreach {
              case e => ship1.drawEngine(e, ship1_coord_diff)
            }
            ship2.engines.foreach {
              case e => ship2.drawEngine(e, ship2_coord_diff, ship2_rotation_diff)
            }

            if (OrbitalKiller.globalScale >= 0.8) {
              if(is_player) {
                if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
                  shipCloser1Km.foreach(s => nearestDockingPoints(s.coord).foreach(dp => {
                    drawFilledCircle(dp.p1, 0.3, colorIfPlayerAliveOrRed(RED))
                    drawFilledCircle(dp.p2, 0.3, colorIfPlayerAliveOrRed(RED))
                  }))
                }
              } else {
                if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
                  shipCloser1Km.foreach(s => nearestDockingPoints(s.coord).foreach(dp => {
                    val (p1_on_the_right_way, p2_on_the_right_way) = {
                      shipCloser1Km.flatMap(_.nearestDockingPoints(coord).map(_.pointsOnTheRightWay(dp))).getOrElse((false, false))
                    }

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
                  }))
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
            openglRotateDeg(rotation)
            openglMove(ship2_coord_diff)
            drawSlidingLines(ship2_draw_points, WHITE)
          }
        }
      }
    }
  }
}
