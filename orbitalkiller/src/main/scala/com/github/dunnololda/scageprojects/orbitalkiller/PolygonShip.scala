package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.elements.OtherShipInfo

import scala.collection.mutable.ArrayBuffer

sealed trait FlightMode {
  def rusStr: String
}

case object FreeFlightMode extends FlightMode {
  override def rusStr: String = "свободный"
}

// 1
case object Killrot extends FlightMode {
  override def rusStr: String = "запрет вращения"
}

// 2
//case object AxisAligned             extends FlightMode
case object VelocityAligned extends FlightMode {
  override def rusStr: String = "ориентация по траектории"
}

// 3
case object OppositeVelocityAligned extends FlightMode {
  override def rusStr: String = "ориентация против траектории"
}

// shift+3
case object CirclularOrbit extends FlightMode {
  override def rusStr: String = "выход на круговую орбиту"
}

// 4
case object NearestShipVelocity extends FlightMode {
  override def rusStr: String = "уравнять скорость с кораблем"
}

// 5
case object NearestShipAligned extends FlightMode {
  override def rusStr: String = "ориентация на корабль"
}

// 6
case object NearestShipAutoDocking extends FlightMode {
  override def rusStr: String = "стыковка с кораблем"
}

// 7
case object NearestPlanetVelocity extends FlightMode {
  override def rusStr: String = "уравнять скорость с ближайшей планетой"
}

// 8
//case object AbsoluteStop            extends FlightMode
case object Maneuvering extends FlightMode {
  override def rusStr: String = "маневрирование"
}

// 0

class DockingPoints(val p1: DVec, val p2: DVec, ship: PolygonShip, val disabled_engine: Option[Int]) {
  val joint_point = p1 + (p2 - p1) * 0.5
  val dock_dist = 0.5

  // в метрах, при каком расстоянии между точками стыковки двух кораблей происходит захватю Для простоты это значение - одинаковая для всех константа. Вынесли сюда, чтобы было одно место, где поменять.
  def curP1 = ship.currentState.coord + p1.rotateDeg(ship.currentState.ang)

  def curP1vel = ship.currentState.vel + (ship.currentState.ang_vel * p1.rotateDeg(90))

  def curP2 = ship.currentState.coord + p2.rotateDeg(ship.currentState.ang)

  def curP2vel = ship.currentState.vel + (ship.currentState.ang_vel * p2.rotateDeg(90))

  /**
   * Стыковочные точки находятся на достаточном расстоянии друг от друга, чтобы состыковаться
   * @param other_ship_docking_points - стыковочные точки другого корабля
   * @return
   */
  def pointsMatch(other_ship_docking_points: DockingPoints): Boolean = {
    curP1.dist(other_ship_docking_points.curP1) < dock_dist && curP2.dist(other_ship_docking_points.curP2) < dock_dist
  }

  /**
   * Наши стыковочные точки лежат на линиях стыковки. Если дальше двигаться в сторону точек стыковки другого корабля, то состыкуемся
   * @param dp - стыковочные точки другого корабля
   * @return
   */
  def pointsOnTheRightWay(dp: DockingPoints): (Boolean, Boolean) = {
    val vv1 = (dp.curP1 - dp.curP2).n * dock_dist
    val vv2 = vv1.perpendicular

    val p1_on_the_right_way = checkAllConditions(
      () => (curP1 - (dp.curP1 + vv1)).perpendicular * vv2 < 0 && (curP1 - (dp.curP1 - vv1)).perpendicular * vv2 > 0 /*,     // p1_inside_line
      () => {                                                                                                         // p1_norm_speed
        val x = dp.curP1vel - curP1vel
        x * (curP1 - dp.curP1) > 0 && x.norma < 10
      }*/
    )
    val p2_on_the_right_way = checkAllConditions(
      () => (curP2 - (dp.curP2 + vv1)).perpendicular * vv2 < 0 && (curP2 - (dp.curP2 - vv1)).perpendicular * vv2 > 0 /*, // p2_inside_line
      () => {                                                                                                         // p2_norm_speed
        val x = dp.curP2vel - curP2vel
        x * (curP2 - dp.curP2) > 0 && x.norma < 10
      }*/
    )
    (p1_on_the_right_way, p2_on_the_right_way)
  }
}

case class DockData(dock_to_ship: PolygonShip, joints: List[Joint], our_dp: DockingPoints, other_ship_dp: DockingPoints)

abstract class PolygonShip(
                            val index: Int,
                            val name: String,
                            init_coord: DVec,
                            init_velocity: DVec = DVec.dzero,
                            init_rotation: Double = 0,
                            ship_designer:Boolean,
                            create_interface:Boolean) {
  println(s"$name -> $index")
  var selected_engine: Option[Engine] = None

  def isSelectedEngine(e: Engine): Boolean = {
    selected_engine.exists(x => x == e)
  }

  def switchEngineSelected(engine_code: Int) {
    engines_mapping.get(engine_code).foreach(e => if (isSelectedEngine(e)) selected_engine = None else selected_engine = Some(e))
  }

  def engines: List[Engine]

  def engines_mapping: Map[Int, Engine]

  /*def switchEngineActive(engine_code:Int) {
    //timeMultiplier = realtime
    engines_mapping.get(engine_code).foreach(e => e.switchActive())
  }*/

  def selectOrSwitchEngineActive(engine_code: Int) {
    //timeMultiplier = realtime
    if (!dockData.exists(d => d.our_dp.disabled_engine.exists(_ == index))) {
      engines_mapping.get(engine_code).foreach(e => {
        if (selected_engine.exists(_ == e)) {
          e.switchActive()
        } else {
          selected_engine = Some(e)
        }
      })
    }
  }

  def mass: Double

  def fuelMass: Double

  def fuelMass_=(m: Double): Unit

  def engine_size: Double

  def convex_parts: List[PolygonShape]

  def wreck_parts: List[PolygonShape]

  def docking_points: List[DockingPoints]

  private var dock_data: Option[DockData] = None

  def dockData = dock_data

  def isDocked: Boolean = dock_data.nonEmpty

  def isDockedToShip(other_ship: PolygonShip): Boolean = dock_data.exists(_.dock_to_ship.index == other_ship.index)

  def notDocked: Boolean = dock_data.isEmpty

  def dock(): Unit = {
    possibleDockPointsWithNearestShip.headOption.foreach {
      case (dp, os, osdp) =>
        /*val joint1 = system_evolution.addJoint(currentState, dp.p1, os.currentState, osdp.p1)
        val joint2 = system_evolution.addJoint(currentState, dp.p2, os.currentState, osdp.p2)
        val joints = List(joint1, joint2)*/
        val joint = system_evolution.addJoint(currentState, dp.joint_point, os.currentState, osdp.joint_point)
        val joints = List(joint)
        setDocked(Some(DockData(os, joints, dp, osdp)))
        os.setDocked(Some(DockData(this, joints, osdp, dp)))
    }
  }

  def undock(): Unit = {
    dock_data.foreach {
      case DockData(os, joints, our_dp, other_ship_dp) =>
        joints.foreach(system_evolution.removeJoint)
        os.setDocked(None)
    }
    dock_data = None
  }

  def setDocked(d: Option[DockData]): Unit = {
    dock_data = d
  }

  def isLanded: Boolean = {
    orbitData.exists(or => {
      checkAllConditions(
        () => coord.dist(or.planet_coord) - or.planet.radius < radius,
        () => ((linearVelocity - or.planet_vel) * (coord - or.planet_coord).n).abs < 0.5,
        () => (((linearVelocity - or.planet_vel) * (coord - or.planet_coord).p) / coord.dist(or.planet_coord) * or.planet.radius - or.planet.groundSpeedMsec).abs < 0.5)
    })
  }

  def isLandedOnPlanet(planet: CelestialBody): Boolean = {
    checkAllConditions(
      () => coord.dist(planet.coord) - planet.radius < radius,
      () => ((linearVelocity - planet.linearVelocity) * (coord - planet.coord).n).abs < 0.5,
      () => (((linearVelocity - planet.linearVelocity) * (coord - planet.coord).p) / coord.dist(planet.coord) * planet.radius - planet.groundSpeedMsec).abs < 0.5
    )
  }

  def coord = if (isAlive) currentState.coord else main_ship_wreck.headOption.map(_.coord).getOrElse(currentState.coord)

  def linearVelocity = if (isAlive) currentState.vel else main_ship_wreck.headOption.map(_.linearVelocity).getOrElse(currentState.vel)

  def relativeLinearVelocity = {
    linearVelocity - orbitData.map(_.planet_vel).getOrElse(DVec.zero)
  }

  def velocityStr: String = {
    orbitData match {
      case Some(or) =>
        s"${msecOrKmsec((linearVelocity - or.planet_vel).norma)} (${or.planet.name}), [b${msecOrKmsec(linearVelocity.norma)} (абсолютная)]"
      case None =>
        s"${msecOrKmsec(linearVelocity.norma)} (абсолютная)"
    }
  }

  def angularVelocity = if (isAlive) currentState.ang_vel else main_ship_wreck.headOption.map(_.angularVelocity).getOrElse(currentState.ang_vel)

  def rotation = if (isAlive) currentState.ang else main_ship_wreck.headOption.map(_.rotation).getOrElse(currentState.ang)

  def currentReactiveForce(time: Long, bs: BodyState): DVec = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(DVec.dzero) {
      case (sum, e) => sum + e.force.rotateDeg(bs.ang)
    }
  }

  def currentReactiveForce(tacts: Long, bs: MutableBodyState): DVec = {
    engines.filter(e => e.active && tacts < e.stopMomentTacts).foldLeft(DVec.dzero) {
      case (sum, e) => sum + e.force.rotateDeg(bs.ang)
    }
  }

  def fuelConsumptionPerTact: Double = {
    engines.filter(e => e.active).foldLeft(0.0) {
      case (sum, e) => sum + e.fuelConsumptionPerTact
    }
  }

  def fuelMassWhenEnginesOff: Double = {
    fuelMass - engines.filter(e => e.active).map(e => e.workTimeTacts * e.fuelConsumptionPerTact).sum
  }

  def fuelMassWhenEnginesOffWithoutEngine(ee: Engine): Double = {
    fuelMass - engines.filter(e => e.active && e != ee).map(e => e.workTimeTacts * e.fuelConsumptionPerTact).sum
  }

  def currentMass(time: Long, bs: BodyState): Double = {
    mass - engines.filter(e => e.active).foldLeft(0.0) {
      case (sum, e) =>
        sum + e.fuelConsumptionPerTact * (math.min(time, e.stopMomentTacts) - (e.stopMomentTacts - e.workTimeTacts))
    }
  }

  def currentMass(time: Long): Double = {
    mass - engines.filter(e => e.active).foldLeft(0.0) {
      case (sum, e) =>
        sum + e.fuelConsumptionPerTact * (math.min(time, e.stopMomentTacts) - (e.stopMomentTacts - e.workTimeTacts))
    }
  }

  def currentTorque(time: Long, bs: BodyState): Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) {
      case (sum, e) => sum + e.torque
    }
  }

  def currentTorque(time: Long, bs: MutableBodyState): Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) {
      case (sum, e) => sum + e.torque
    }
  }

  def currentTorque(time: Long): Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) {
      case (sum, e) => sum + e.torque
    }
  }

  def deactivateAllEngines(): Unit = {
    engines.foreach(_.active = false)
  }

  def activateOnlyTheseEngines(engines_to_activate: Engine*) {
    //timeMultiplier = realtime
    engines_to_activate.foreach(_.active = true)
    engines.withFilter(e => e.active && !engines_to_activate.contains(e)).foreach(_.active = false)
  }

  def activateOnlyOneEngine(e: Engine) {
    activateOnlyTheseEngines(e)
  }

  def allEnginesInactive: Boolean = {
    engines.forall(!_.active)
  }

  def engineColor(e: Engine, in_shadow: Boolean): ScageColor = {
    if (is_dead || e.active) RED else if (in_shadow) DARK_GRAY else WHITE
  }

  def engineActiveSize(e: Engine, max_size: Double): Double = {
    max_size * e.power / e.max_power
  }

  protected def drawEngine(e: Engine) {
    if (!dock_data.exists(_.our_dp.disabled_engine.exists(_ == e.index))) {
      val is_vertical = e.force_dir.x == 0
      val (center, width, height) = e.force_dir match {
        case DVec(0, -1) => (e.position + DVec(0, 0.25) * engine_size, 1 * engine_size, 0.5 * engine_size)
        case DVec(0, 1) => (e.position + DVec(0, -0.25) * engine_size, 1 * engine_size, 0.5 * engine_size)
        case DVec(-1, 0) => (e.position + DVec(0.25, 0) * engine_size, 0.5 * engine_size, 1 * engine_size)
        case DVec(1, 0) => (e.position + DVec(-0.25, 0) * engine_size, 0.5 * engine_size, 1 * engine_size)
        case _ => throw new Exception("engine force dir other than vertical or horizontal is not supported")
      }

      val in_shadow = {
        // ниже код вычисляет, в тени находится двигатель или нет
        /*val curP = coord + e.position.rotateDeg(rotation)
      inShadowOfPlanet(curP).nonEmpty || curDrawLines.filterNot(x => math.abs((x(1)._1 - x(0)._1).perpendicular*(curP - x(0)._1)) < 0.1).exists(x => {
        val res = areLinesIntersect(curP, sun.coord, x(0)._1, x(1)._1)
        /*if(res) {
          if(e.index == "6" || e.index == "9")
          println(s"${e.index} intersects with line ${x(0)._2} - ${x(1)._2}")
        }*/
        //println(s"areLinesIntersect($curP1, $cur_sun_coord, ${x(0)}, ${x(1)}) = $res")
        res
      })*/
        false
      }

      drawRectCentered(center, width, height, color = engineColor(e, in_shadow))
      if (isSelectedEngine(e)) drawRectCentered(center, width * 1.5, height * 1.5, color = engineColor(e, in_shadow))
      if (e.active && e.power > 0) {
        if (is_vertical) {
          drawFilledRectCentered(center, engineActiveSize(e, width), height, color = engineColor(e, in_shadow))
        } else {
          drawFilledRectCentered(center, width, engineActiveSize(e, height), color = engineColor(e, in_shadow))
        }
      }
      //print(s"${e.index}", e.position.toVec, color = WHITE, size = (max_font_size / globalScale).toFloat)
    }
  }

  protected def drawDashedLine(from: DVec, to: DVec, dash_len: Double, color: ScageColor): Unit = {
    val line_len = (to - from).norma
    val normal = (to - from).n
    (0.0 to line_len - dash_len by dash_len * 2).foreach {
      case dash_from => drawLine(from + normal * dash_from, from + normal * (dash_from + dash_len), color)
    }
  }

  protected def drawShip(): Unit = {
    if (!drawMapMode && coord.dist2(player_ship.coord) < 100000 * 100000) {
      if (isAlive) {
        openglLocalTransform {
          openglMove(coord - base)
          /*drawFilledCircle(DVec.zero, 2, GREEN) // mass center
          if (OrbitalKiller.globalScale >= 0.8) {
            drawArrow(DVec.zero, relativeLinearVelocity.n * radius, CYAN) // current velocity
          }*/

          // ниже рисуем aabb и кружочки вокруг формы корабля и отдельных частей формы
          /*val x = currentState.shape.asInstanceOf[PolygonShape]
          drawCircle(DVec.zero, x.radius, WHITE)
          drawRectCentered(DVec.zero, x.radius*2, x.radius*2, WHITE)
          x.convex_parts.foreach(y => {
            drawCircle(y.points_center.rotateDeg(rotation), y.points_radius, WHITE)
            drawRectCentered(y.points_center.rotateDeg(rotation), y.points_radius*2, y.points_radius*2, WHITE)
          })*/

          openglRotateDeg(rotation)
          drawSlidingLines(draw_points, WHITE)

          if (OrbitalKiller.globalScale >= 0.8) {
            if (isDocked) {
              dockData.foreach(d => {
                drawFilledCircle(d.our_dp.p1, 0.3, colorIfPlayerAliveOrRed(GREEN))
                drawFilledCircle(d.our_dp.p2, 0.3, colorIfPlayerAliveOrRed(GREEN))
              })
            } else if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
              docking_points.foreach(dp => {
                val (p1_on_the_right_way, p2_on_the_right_way) = OrbitalKiller.player_ship.docking_points.headOption.map(_.pointsOnTheRightWay(dp)).getOrElse((false, false))

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

          engines.foreach {
            case e => drawEngine(e)
          }
        }
      } else {
        openglLocalTransform {
          openglMove(coord - base)
          openglRotateDeg(rotation)
          drawSlidingLines(draw_points, colorIfPlayerAliveOrRed(WHITE))
        }
      }
    }
  }

  private val render_id = render {
    drawShip()
  }

  def preserveAngularVelocity(ang_vel_deg: Double)

  def preserveVelocity(vel: DVec)

  /**
   * rotation и angle_deg оба в диапазоне 0 - 360
   * @param angle_deg - угол между ориентацией корабля и вектором DVec(0, 1), который необходимо поддерживать
   */
  def preserveAngle(angle_deg: Double) {
    if (rotation != angle_deg) {
      if (rotation > angle_deg) {
        if (rotation - angle_deg < angle_deg - rotation + 360) {
          val diff = rotation - angle_deg
          if (diff > 50) preserveAngularVelocity(-10)
          else if (diff > 10) preserveAngularVelocity(-5)
          else if (diff > 1) preserveAngularVelocity(-1.8)
          else if (diff > 0.1) preserveAngularVelocity(-0.2)
          else preserveAngularVelocity(0)
        } else {
          val diff = angle_deg - rotation + 360
          if (diff > 50) preserveAngularVelocity(10)
          else if (diff > 10) preserveAngularVelocity(5)
          else if (diff > 1) preserveAngularVelocity(1.8)
          else if (diff > 0.1) preserveAngularVelocity(0.2)
          else preserveAngularVelocity(0)
        }
      } else if (rotation < angle_deg) {
        if (rotation - angle_deg > angle_deg - rotation - 360) {
          val diff = rotation - angle_deg
          if (diff < -50) preserveAngularVelocity(10)
          else if (diff < -10) preserveAngularVelocity(5)
          else if (diff < -1) preserveAngularVelocity(1.8)
          else if (diff < -0.1) preserveAngularVelocity(0.2)
          else preserveAngularVelocity(0)
        } else {
          val diff = angle_deg - rotation - 360
          if (diff < -50) preserveAngularVelocity(-10)
          else if (diff < -10) preserveAngularVelocity(-5)
          else if (diff < -1) preserveAngularVelocity(-1.8)
          else if (diff < -0.1) preserveAngularVelocity(-0.2)
          else preserveAngularVelocity(0)
        }
      }
    }
  }

  protected val correction_check_period = 180
  protected var last_correction_or_check_moment: Long = 0l

  private var prev_flight_mode_and_engine_states: Option[(FlightMode, List[(Long, Double, Boolean)])] = None

  def haveSavedFlightMode = prev_flight_mode_and_engine_states.nonEmpty

  def saveFlightModeAndEngineStates(prev_flight_mode: FlightMode): Unit = {
    prev_flight_mode_and_engine_states = Some((prev_flight_mode, engines.map(e => (e.workTimeTacts, e.power, e.active))))
  }

  def restoreFlightModeAndEngineStates(): Unit = {
    prev_flight_mode_and_engine_states match {
      case Some((prev_flight_mode, engine_states)) =>
        flightMode = prev_flight_mode
        engines.zip(engine_states).foreach {
          case (e, (tacts, power, active)) =>
            e.active = active
            e.workTimeTacts = tacts
            e.power = power
        }
        prev_flight_mode_and_engine_states = None
      case None =>
    }
  }

  private var flight_mode: FlightMode = FreeFlightMode

  def flightMode: FlightMode = flight_mode

  def flightMode_=(new_flight_mode: FlightMode) {
    val prev_flight_mode = flight_mode
    flight_mode = new_flight_mode
    last_correction_or_check_moment = 0l
    if (flight_mode == Maneuvering) {
      val ten_min_or_max_time_at_full_power = math.min((fuelMass / engines.map(_.maxFuelConsumptionPerTact).max).toLong, 37800)
      if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
        engines.foreach(e => e.power = 10000)
      } else {
        engines.foreach(e => e.power = {
          if (InterfaceHolder.gSwitcher.maxGSet) {
            math.min(
              (mass + dockData.map(_.dock_to_ship.mass).getOrElse(0.0)) * InterfaceHolder.gSwitcher.maxG * OrbitalKiller.earth.g + {
                earth.airResistance(currentState, earth.currentState, ShipsHolder.currentShipStatesExceptShip(index), 28, 0.5).norma
              },
              e.max_power * 0.5)
          } else {
            e.max_power * 0.5
          }
        })
      }
      engines.filterNot(_.active).foreach(e => e.workTimeTacts = ten_min_or_max_time_at_full_power) // 10 minutes in tacts (10*60*63)
      val active_engines = engines.filter(_.active)
      if (active_engines.map(ae => ae.fuelConsumptionPerTact * ten_min_or_max_time_at_full_power).sum <= fuelMass) {
        active_engines.foreach(e => e.workTimeTacts = ten_min_or_max_time_at_full_power)
      } else {
        val fuel_for_every_active_engine = fuelMass / active_engines.length
        active_engines.foreach(e => e.workTimeTacts = (fuel_for_every_active_engine / e.fuelConsumptionPerTact).toLong)
      }
    } else {
      if (flight_mode == FreeFlightMode) {
        engines.foreach(e => e.active = false)
      } else if (prev_flight_mode == Maneuvering &&
        (flight_mode == Killrot ||
          flight_mode == NearestShipVelocity ||
          flight_mode == NearestShipAligned ||
          flight_mode == VelocityAligned ||
          flight_mode == OppositeVelocityAligned)) {
        saveFlightModeAndEngineStates(prev_flight_mode)
      } else if (flight_mode == NearestPlanetVelocity) {
        vertical_speed_msec = 0
        horizontal_speed_msec = 0
      }
      if (prev_flight_mode == Maneuvering && flight_mode != Maneuvering) {
        engines.foreach(e => e.workTimeTacts = 0)
      }
    }
  }

  def vertical_speed_msec: Int = 0

  def vertical_speed_msec_=(x: Int) {}

  def horizontal_speed_msec: Int = 0

  def horizontal_speed_msec_=(x: Int) {}

  def flightModeStr: String = flight_mode match {
    case NearestPlanetVelocity => s"уравнять скорость с ближайшей планетой: ${msecOrKmsec(vertical_speed_msec)}, ${msecOrKmsec(horizontal_speed_msec)}"
    case x => x.rusStr
  }

  /**
   * Все другие корабли, отсортированные по расстоянию по убыванию (первый - ближайший).
   * @return
   */
  def shipsNear: Seq[PolygonShip] = ShipsHolder.ships.filter(s => s.index != index && s.isAlive).sortBy(s => coord.dist2(s.coord))

  /**
   * Корабли ближе x км от нас. Метод используется для вычисления автоматического наведения ракет.
   * @param x - расстояние в километрах
   * @return
   */
  def shipsCloserXKm(x: Long): Seq[PolygonShip] = ShipsHolder.ships.filter(s => s.index != index && s.isAlive && s.coord.dist2(coord) < x * 1000l * x * 1000l).sortBy(s => coord.dist2(s.coord))

  /**
   * Корабль ближе x км от нас. Если таких несколько, то ближайший
   * @param x - дистанция в километрах
   * @return
   */
  def shipCloserXKm(x: Long): Option[PolygonShip] = shipsCloserXKm(11).headOption

  /**
   * Корабль ближе 1 км от нас. Если таких несколько, то ближайший.
   * Метод используется для реализации автоматической стыковки.
   * @return
   */
  def shipCloser1Km: Option[PolygonShip] = shipCloserXKm(1)

  /**
   * Корабль ближе 500 км от нас, интерфейс которого не свернут. Если таких несколько, то ближайший.
   * Метод используется в алогритмах автоматического уравнивания скорости, поддержания направления, стыковки
   * @return
   */
  def shipCloser500KmNonMinimized: Option[PolygonShip] = {
    ShipsHolder.ships.filter(s => {
      s.index != index &&
      s.isAlive &&
      s.coord.dist2(coord) < 500 * 1000l * 500 * 1000l &&
      s.shipInterface.exists(!_.isMinimized)
    }).sortBy(s => coord.dist2(s.coord)).headOption
  }

  /**
   * Можем ли состыковаться с ближайшим кораблем
   * @return
   */
  def canDockWithNearestShip: Boolean = {
    shipCloser1Km.exists(os => {
      docking_points.exists(dp => {
        os.docking_points.exists(osdp => dp.pointsMatch(osdp))
      })
    })
  }

  def possibleDockPointsWithNearestShip: List[(DockingPoints, PolygonShip, DockingPoints)] = {
    for {
      dp <- docking_points
      os <- shipCloser1Km.toList
      osdp <- os.docking_points
      if dp.pointsMatch(osdp)
    } yield (dp, os, osdp)
  }

  def canDockWithNearestShipUsingDockPoints(dp: DockingPoints): Boolean = {
    shipCloser1Km match {
      case Some(os) => os.docking_points.exists(osdp => dp.pointsMatch(osdp))
      case None => false
    }
  }

  val pilot_mass = 75
  val pilot_position = DVec(0, 8)
  private var pilot_average_g: Double = 0.0
  private val pilot_accs = ArrayBuffer[(DVec, Long)]()

  def is_manned: Boolean

  private var is_dead = false
  private var death_reason = ""

  def deathReason = death_reason

  def isDead = is_dead

  def isAlive = !is_dead

  private var ship_is_crashed = false

  def isCrashed = ship_is_crashed

  /**
   * Если на пилота действует перегрузка, уменьшается этот счетчик. Скорость его уменьшения зависит от величины перегрузки.
   * Если счетчик дойдет до нуля, пилот умирает. При отсутствии перегрузки (ускорение 1g или меньше), счетчик примерно за 30 секунд
   * возвращается до 100.
   */
  private var before_death_counter: Double = 100

  /**
   * С какой скоростью уменьшается счетчик before_death_counter в зависимости от ускорения, которое испытывает пилот.
   * Размерность: ед/такт (один такт - 1/63 секунды).
   * Если перегрузка меньше либо равна 1.09g - пилот восстанавливается после перегрузки (rate отрицательный) с 0 за примерно 60 секунд.
   * Если перегрузка больше 1.09, но меньше либо равна 4g - rate нулевой (не восстанавливаемся, но и не приближаемся к смерти).
   * Если перегрузка больше 4g - rate положительный (может дойти до нуля).
   * @param gs - перегрузка в единицах g
   * @return
   */
  private def deatchCounterChangeRate(gs: Double): Double = {
    def _linearFunc(p1: (Double, Double), p2: (Double, Double)): Double = {
      val a = (p1._2 - p2._2) / (p1._1 - p2._1)
      val b = p1._2 - a * p1._1
      val sec = a * gs + b // за столько секунд счетчик дойдет от ста до нуля при данном значении перегрузки
      100.0 / sec
    }

    // https://en.wikipedia.org/wiki/G-force#Human_tolerance_of_g-force
    // http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19980223621.pdf (p. 30, fig. 8)
    val ans = if (gs <= 1.09) {
      -100.0 / 60
    } else if (gs <= 4) {
      0
    } else if (gs <= 6) {
      _linearFunc((4, 660), (6, 240))
    } else if (gs <= 8) {
      _linearFunc((6, 240), (8, 60))
    } else if (gs <= 10) {
      _linearFunc((8, 60), (10, 24))
    } else if (gs <= 11) {
      _linearFunc((10, 24), (11, 6))
    } else if (gs <= 20) {
      _linearFunc((11, 6), (20, 1))
    } else if (gs <= 27) {
      _linearFunc((20, 1), (27, 0.3))
    } else 100.0 / 0.01
    ans * base_dt
  }

  def colorIfAliveOrRed(color: => ScageColor) = if (isDead) RED else color

  def colorIfPlayerAliveOrRed(color: => ScageColor) = if (OrbitalKiller.player_ship.isDead) RED else color

  protected var main_ship_wreck: Option[Wreck] = None

  def kill(reason: String, crash: Boolean): Unit = {
    is_dead = true
    death_reason = reason
    flightMode = FreeFlightMode
    engines.foreach(_.active = false)
    /*if(this.index == OrbitalKiller.ship.index) {
      viewMode = FixedOnShipAbsolute
    }*/
    if (isDocked) {
      undock()
    }
    if (crash) {
      ShipsHolder.removeShip(this)
      delOperation(render_id)
      val wrecks = wreck_parts.zipWithIndex.map { case (wreck_part, idx) =>
        val part_center = currentState.coord + wreck_part.points.sum / wreck_part.points.length
        val part_points = wreck_part.points.map(p => currentState.coord + p - part_center)
        val maybe_obstacle = currentState.contacts.headOption.map(c => if (c.a.index != index) c.a else c.b)
        val random_wreck_vel_func = wreckRandomVelocity(maybe_obstacle)
        new Wreck(mass / wreck_parts.length,
          part_center,
          random_wreck_vel_func(),
          rotation,
          part_points,
          is_main = idx == 0 && index == player_ship.index)
      }
      main_ship_wreck = wrecks.find(_.is_main)
      ship_is_crashed = true
    }
  }

  private def wreckRandomVelocity(maybe_obstacle: Option[MutableBodyState]): () => DVec = {
    maybe_obstacle match {
      case Some(obstacle) =>
        ShipsHolder.shipByIndex(obstacle.index) match {
          case Some(ship_obstacle) =>
            val dir_deg = 140.0 + math.random * 80.0
            //val new_vel = ((mass - ship_obstacle.mass)*linearVelocity + 2*ship_obstacle.mass*ship_obstacle.linearVelocity)/(mass + ship_obstacle.mass)
            val new_vel = linearVelocity * mass / (mass + obstacle.mass) + obstacle.vel * obstacle.mass / (mass + obstacle.mass)
            () => new_vel + (linearVelocity - ship_obstacle.linearVelocity).n.rotateDeg(dir_deg) * 30.0
          case None =>
            planetByIndex(obstacle.index) match {
              case Some(planet_obstacle) =>
                val dir_deg = 140.0 + math.random * 80.0
                val obstacle_vel = obstacle.vel + (coord - obstacle.coord).p * planet_obstacle.groundSpeedMsec
                () => obstacle_vel + (planet_obstacle.coord - coord).n.rotateDeg(dir_deg) * 50.0
              case None =>
                val dir_deg = math.random * 360
                () => linearVelocity + DVec(0, 1).rotateDeg(dir_deg) * 30.0
            }
        }
      case None =>
        val dir_deg = math.random * 360
        () => linearVelocity + DVec(0, 1).rotateDeg(dir_deg) * 30.0
    }
  }

  def pilotStateStr: String = {
    if (is_manned) {
      if (!is_dead) {
        if (pilot_average_g < 0.1) {
          if (before_death_counter != 100) {
            val rate = 100.0 / 60 * base_dt // восстановление после критической перегрузки за 60 секунд
            val time_to_restore_msec = (((100 - before_death_counter) / rate) * base_dt * 1000).toLong
            f"Пилот в состоянии невесомости. Восстанавливается после перегрузки (${timeStr(time_to_restore_msec)})"
          } else {
            "Пилот в состоянии невесомости"
          }
        } else if (pilot_average_g <= 1.09) {
          if (before_death_counter != 100) {
            val rate = 100.0 / 60 * base_dt // восстановление после критической перегрузки за 60 секунд
            val time_to_restore_msec = (((100 - before_death_counter) / rate) * base_dt * 1000).toLong
            f"Пилот испытывает силу тяжести $pilot_average_g%.1fg. Восстанавливается после перегрузки (${timeStr(time_to_restore_msec)})"
          } else {
            f"Пилот испытывает силу тяжести $pilot_average_g%.1fg"
          }
        } else {
          if (pilot_average_g > 4) {
            val rate = deatchCounterChangeRate(pilot_average_g)
            val time_before_death_msec = ((before_death_counter / rate) * base_dt * 1000).toLong
            f"[rПилот испытывает критическую перегрузку $pilot_average_g%.1fg. Смерть через ${timeStr(time_before_death_msec)}]"
          } else {
            if (before_death_counter != 100) {
              f"Пилот испытывает перегрузку $pilot_average_g%.1fg. [oТребуется отдых после критической перегрузки]"
            } else {
              f"Пилот испытывает перегрузку $pilot_average_g%.1fg"
            }
          }
        }
      } else {
        death_reason
      }
    } else {
      "N/A"
    }
  }

  def massStr = f"Масса корабля: ${gOrKg(mass)}. Остаток топлива: ${gOrKg(fuelMass)}"

  def shadowSideStr = {
    inShadowOfPlanet(coord) match {
      case Some((planet, planet_state)) =>
        /*planet_name match {
          case moon.name => "Корабль находится в тени Луны"
          case earth.name => "Корабль находится в тени Земли"
          case _ => "Корабль находится в тени"
        }*/
        "Корабль находится в тени"
      case None =>
        "Корабль освещается солнцем"
    }
  }

  /**
   * Точки, обозначающие корпус корабля. Задаются координатами относительно центра масс. Фактическую координату точки в данный момент времени
   * можно получить, повернув на текущий угол и прибавив координату ц.м.
   * @return
   */
  def points: List[DVec]

  lazy val draw_points = points :+ points.head

  /**
   * Точки, обозначающие корпус корабля, группируются по два, получаются отрезки. Для каждой точки вычисляется фактическая текущая координата.
   * Получается набор фактических текущих координат корпуса, собранных по два.
   * @return List<List<DVec>> - Во внутреннем списке всегда два элемента.
   */
  def curDrawLines = draw_points.zipWithIndex.sliding(2).map {
    case List((p1, p1idx), (p2, p2idx)) =>
      List((coord + p1.rotateDeg(rotation), p1idx), (coord + p2.rotateDeg(rotation), p2idx))
  }.toList


  lazy val radius: Double = {
    points.map(_.norma).max
  }

  lazy val initState: BodyState = BodyState(
    index,
    mass,
    acc = DVec.zero,
    vel = init_velocity,
    coord = init_coord,
    ang_acc = 0,
    ang_vel = 0,
    ang = init_rotation,
    shape = PolygonShape(points, convex_parts),
    is_static = false)

  lazy val currentState: MutableBodyState = initState.toMutableBodyState
  private var ship_interface:Option[OtherShipInfo] = None
  def shipInterface:Option[OtherShipInfo] = ship_interface
  if(!ship_designer) {
    ShipsHolder.addShip(this)
    if(create_interface) {
      ship_interface = Some(InterfaceHolder.addShipInterface(this))
    }
  }

  var orbitData: Option[OrbitData] = None

  def beforeStep(): Unit = {
    if(currentState.active) {
      engines.foreach(e => {
        if (e.active) {
          if (e.workTimeTacts <= 0 || player_ship.fuelMass <= 0) {
            e.active = false
          } else {
            if (e.ship.fuelMass - e.fuelConsumptionPerTact <= 0) {
              e.active = false
            }
          }
        }
      })
      if (currentState.ang_vel != 0 && math.abs(currentState.ang_vel) < OrbitalKiller.angular_velocity_error) {
        currentState.ang_vel = 0
      }
      currentState.mass = mass /*currentMass(_tacts)*/
    }
  }

  protected var deactivate_moment_sec:Long = 0l
  protected var deactivate_point_relative:DVec = DVec.zero

  private def _afterStep(time_msec:Long): Unit = {
    // сила от реактивных двигателей и сила сопротивления воздуха
    val reactive_force = currentReactiveForce(0, currentState) + {
      earth.airResistance(currentState, earth.currentState, ShipsHolder.currentShipStatesExceptShip(index), 28, 0.5)
    }
    if (!ship_is_crashed) {
      val dvel = currentState.dvel.norma
      if (dvel > 10) {
        // crash tolerance = 10 m/s
        val crash_g = dvel / OrbitalKiller.base_dt / earth.g
        kill(f"Корабль уничтожен в результате столкновения ($crash_g%.2fg)", crash = true)
        return
      } else {
        // ниже мы рассчитаем отдельно вертикальную и горизонтальную перегрузки и потом сложим их. Так надо считать, потому что к вертикальной перегрузке прибавляется центробежная сила, а к горизонтальной нет.
        val v_vert = pilot_position.rotateDeg(rotation).n // единичный вектор спина-грудь пилота
        val v_hor = -v_vert.perpendicular // единичный вектор левая рука - права рука пилота
        // центробежная сила от вращения корабля
        val centrifugial_force = if (angularVelocity == 0) 0.0 else pilot_mass * math.pow(angularVelocity.toRad, 2) * pilot_position.norma
        // reactive_force берем с минусом, потому что пилота вжимает под действием этой силы в противоположную сторону. Аналогично ускорение от коллизий
        val pilot_acc_vert = -reactive_force / mass * v_vert + centrifugial_force / pilot_mass - currentState.dacc * v_vert
        val pilot_acc_hor = -reactive_force / mass * v_hor - currentState.dacc * v_hor
        val pilot_acc = pilot_acc_vert * DVec(0, 1) + pilot_acc_hor * DVec(1, 0) // тут мы умножаем на единичные векторы в системе координат: начало в центре масс, вертикальный вектор - от центра масс к пилоту
        pilot_accs += ((pilot_acc, time_msec))
        if (time_msec - pilot_accs.head._2 >= 1000) {
          pilot_average_g = (pilot_accs.map(_._1).sum / pilot_accs.length).norma / earth.g
          pilot_accs.clear()
        }
      }
      // если провалились сквозь землю
      currentPlanetStates.find {
        case (planet, planet_state) => planet.coord.dist(currentState.coord) < planet.radius
      }.foreach {
        case (planet, planet_state) =>
          currentState.coord = currentState.coord + (currentState.coord - planet.coord).n * (planet.radius + radius - planet.coord.dist(currentState.coord))
          currentState.vel = planet.linearVelocity
          kill("Корабль врезался в планету", crash = true)
          return
      }
      // если подлетаем к поверхности Солнца ближе, чем 30 миллионов километров, то бууум!)
      if (coord.dist(sun.coord) - sun.radius < 30000000000l) {
        kill("Корабль слишком приблизился к Солнцу и сгорел", crash = true)
        return
      }
    }
    if (!is_dead) {
      // пилот может испытывать перегрузку больше 4g только ограниченный период времени, потом наступает смерть
      // для беспилотной системы это значение примем 40g (условный показатель)
      if (pilot_average_g > {
        if (is_manned) 4 else 40
      }) {
        // беспилотный корабль может выдерживать 10-кратно большие перегрузки по сравнению с пилотируемым
        val rate = deatchCounterChangeRate({
          if (is_manned) pilot_average_g else pilot_average_g / 10
        })
        before_death_counter -= rate
        if (before_death_counter <= 0) {
          if (is_manned) {
            kill(f"Пилот умер от сильной перегрузки ($pilot_average_g%.2fg)", crash = false)
          } else {
            kill(f"Корабль разрушился от сильной перегрузки ($pilot_average_g%.2fg)", crash = true)
          }
        } else {
          // автоматическое отключение двигателей, если до смерти от перегрузки осталась одна секунда
          val time_before_death_msec = ((before_death_counter / rate) * base_dt * 1000).toLong
          if (time_before_death_msec <= 1000) {
            // 1 секунда до гибели от перегрузки
            if (engines.exists(_.active)) {
              // если работают какие-то двигатели
              flightMode = FreeFlightMode // отключаем двигатели
              engines.foreach(_.active = false)
            }
          }
        }
      } else if (before_death_counter != 100) {
        if (pilot_average_g <= 1.09) {
          // пилот может восстанавливаться после перегрузки, только если текущая ускорение не выше 1.09g
          val rate = 100.0 / 60 * base_dt // восстановление после критической перегрузки за 60 секунд
          before_death_counter += rate
          if (before_death_counter >= 100) {
            before_death_counter = 100
          }
        }
      }
      // автоматическая регулировка мощности двигателей в соответствие с настройкой gSwitcher
      if (InterfaceHolder.gSwitcher.maxGSet && pilot_average_g > InterfaceHolder.gSwitcher.maxG) {
        val active_engines = engines.filter(e => e.active && 0 < e.stopMomentTacts)
        if (active_engines.nonEmpty) {
          val cur_force = reactive_force.norma
          val allowed_force = mass * InterfaceHolder.gSwitcher.maxG * OrbitalKiller.earth.g
          val force_diff = cur_force - allowed_force
          val force_diff_for_engine = force_diff / active_engines.length
          active_engines.foreach(e => if (force_diff_for_engine < e.power) {
            e.power -= force_diff_for_engine
          })
        }
      }
    }
    engines.foreach(e => {
      if (e.active) {
        if (e.workTimeTacts > 0) {
          e.workTimeTacts -= 1
          //InterfaceHolder.enginesInfo.addWorkTime(base_dt*1000*e.power/e.max_power)
          e.ship.fuelMass -= e.fuelConsumptionPerTact
        } else e.active = false
      }
    })
  }

  def afterStep(time_msec: Long): Unit = {
    // условие сделать корабль неактивным и не обрабатывать его:
    // если это не корабль игрока, расстояние от данного корабля до корабля игрока больше 1000 км,
    // перигей орбиты выше верхней границы атмосферы (орбита стабильная), двигатели не включены,
    // интерфейс данного корабля существует и свернут
    val condition = index != player_ship.index &&
      coord.dist2(OrbitalKiller.player_ship.coord) > 1000000l*1000000l &&
      orbitData.exists(or => or.ellipseOrbit.exists(e => e.r_p > or.planet.radius + or.planet.air_free_altitude)) &&
      engines.forall(!_.active) &&
      (ship_interface.isEmpty || ship_interface.exists(_.isMinimized))
    if(currentState.active) {
      _afterStep(time_msec)
      if(condition) {
        currentState.active = false
        deactivate_moment_sec = time_msec/1000
        deactivate_point_relative = coord - orbitData.map(_.planet.coord).getOrElse(DVec.zero)
      }
    } else {
      if(!condition) {
        orbitData match {
          case Some(or) =>
            or.ellipseOrbit match {
              case Some(e) =>
                val new_e = e.withNewFocusPosition(or.planet.coord)
                currentState.coord = new_e.orbitalPointAfterTime(deactivate_point_relative + or.planet.coord, OrbitalKiller.timeMsec/1000 - deactivate_moment_sec, or.ccw)
                val (vt, vr) = new_e.orbitalVelocityInPoint(currentState.coord)
                val r = if(or.ccw) (currentState.coord - or.planet.coord).n else -(currentState.coord - or.planet.coord).n
                val t = r.perpendicular
                currentState.vel = vr * r + vt * t + or.planet.linearVelocity
              case None =>
            }
          case None =>
        }
        currentState.active = true
      }
    }
  }

  def onCollision(): Unit = {
    if (!ship_is_crashed) {
      val dvel = currentState.dvel.norma
      if (dvel > 10) {
        // crash tolerance = 10 m/s
        val crash_g = dvel / base_dt / earth.g
        kill(f"Корабль уничтожен в результате столкновения ($crash_g%.2fg)", crash = true)
      }
    }
  }
}
