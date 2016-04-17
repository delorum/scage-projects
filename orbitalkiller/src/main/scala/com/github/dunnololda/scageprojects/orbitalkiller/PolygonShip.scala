package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{ScageId, DVec}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

import scala.collection.mutable.ArrayBuffer

sealed trait FlightMode {
  def rusStr:String
}

case object FreeFlightMode          extends FlightMode {override def rusStr: String = "свободный"}  // 1
case object Killrot                 extends FlightMode {override def rusStr: String = "запрет вращения"}  // 2
//case object AxisAligned             extends FlightMode
case object VelocityAligned         extends FlightMode {override def rusStr: String = "ориентация по траектории"}  // 3
case object OppositeVelocityAligned extends FlightMode {override def rusStr: String = "ориентация против траектории"}  // shift+3
case object CirclularOrbit          extends FlightMode {override def rusStr: String = "выход на круговую орбиту"}  // 4
case object NearestShipVelocity     extends FlightMode {override def rusStr: String = "уравнять скорость с кораблем"}  // 5
case object NearestShipAligned      extends FlightMode {override def rusStr: String = "ориентация на корабль"}  // 6
case object NearestShipAutoDocking      extends FlightMode {override def rusStr: String = "стыковка с кораблем"}  // 7
case object NearestPlanetVelocity   extends FlightMode {override def rusStr: String = "уравнять скорость с ближайшей планетой"}  // 8
//case object AbsoluteStop            extends FlightMode
case object Maneuvering             extends FlightMode {override def rusStr: String = "маневрирование"}  // 0

class DockingPoints(val p1:DVec, val p2:DVec, ship:PolygonShip) {
  val dock_dist = 0.5 // в метрах, при каком расстоянии между точками стыковки двух кораблей происходит захватю Для простоты это значение - одинаковая для всех константа. Вынесли сюда, чтобы было одно место, где поменять.
  def curP1 = ship.currentState.coord + p1.rotateDeg(ship.currentState.ang)
  def curP1vel = ship.currentState.vel + (ship.currentState.ang_vel*p1.rotateDeg(90))
  def curP2 = ship.currentState.coord + p2.rotateDeg(ship.currentState.ang)
  def curP2vel = ship.currentState.vel + (ship.currentState.ang_vel*p2.rotateDeg(90))
  def pointsMatch(other_ship_docking_points:DockingPoints):Boolean = {
    curP1.dist(other_ship_docking_points.curP1) < dock_dist && curP2.dist(other_ship_docking_points.curP2) < dock_dist
  }
  private def _checkAllConditions(conditions:(() => Boolean)*):Boolean = {
    if(conditions.isEmpty) true
    else if(conditions.head()) _checkAllConditions(conditions.tail:_*)
    else false
  }
  
  def pointsOnTheRightWay(dp:DockingPoints):(Boolean, Boolean) = {
    val vv1 = (dp.curP1-dp.curP2).n*dock_dist
    val vv2 = vv1.perpendicular

    val p1_on_the_right_way = _checkAllConditions(
      () => (curP1 - (dp.curP1 + vv1)).perpendicular*vv2 < 0 && (curP1 - (dp.curP1 - vv1)).perpendicular*vv2 > 0,     // p1_inside_line
      () => {                                                                                                         // p1_norm_speed
        val x = dp.curP1vel - curP1vel
        x * (curP1 - dp.curP1) > 0 && x.norma < 10
      }
    )
    val p2_on_the_right_way = _checkAllConditions(
      () => (curP2 - (dp.curP2 + vv1)).perpendicular * vv2 < 0 && (curP2 - (dp.curP2 - vv1)).perpendicular * vv2 > 0, // p2_inside_line
      () => {                                                                                                         // p2_norm_speed
        val x = dp.curP2vel - curP2vel
        x * (curP2 - dp.curP2) > 0 && x.norma < 10
      }
    )
    (p1_on_the_right_way, p2_on_the_right_way)
  }
}
case class DockData(dock_to_ship:PolygonShip, joints:List[Joint], our_dp:DockingPoints, other_ship_dp:DockingPoints)

abstract class PolygonShip(
  val index:Int,
  val name:String,
  init_coord:DVec,
  init_velocity:DVec = DVec.dzero,
  init_rotation:Double = 0) {

  var selected_engine:Option[Engine] = None
  def isSelectedEngine(e:Engine):Boolean = {
    selected_engine.exists(x => x == e)
  }

  def switchEngineSelected(engine_code:Int) {
    engines_mapping.get(engine_code).foreach(e => if(isSelectedEngine(e)) selected_engine = None else selected_engine = Some(e))
  }

  def engines:List[Engine]
  def engines_mapping:Map[Int, Engine]
  def switchEngineActive(engine_code:Int) {
    //timeMultiplier = realtime
    engines_mapping.get(engine_code).foreach(e => e.switchActive())
  }

  def selectOrSwitchEngineActive(engine_code:Int) {
    //timeMultiplier = realtime
    engines_mapping.get(engine_code).foreach(e => {
      if(selected_engine.exists(_ == e)) {
        e.switchActive()
      } else {
        selected_engine = Some(e)
      }
    })
  }

  def mass:Double

  def fuelMass:Double
  def fuelMass_=(m:Double):Unit

  def convex_parts:List[PolygonShape]
  def wreck_parts:List[PolygonShape]
  def docking_points:List[DockingPoints]

  private var dock_data:Option[DockData] = None
  def dockData = dock_data
  def isDocked:Boolean = dock_data.nonEmpty
  def isDockedToShip(other_ship:PolygonShip):Boolean = dock_data.exists(_.dock_to_ship.index == other_ship.index)
  def notDocked:Boolean = dock_data.isEmpty
  def dock(): Unit = {
    possibleDockPointsWithNearestShip.headOption.foreach {
      case (dp, os, osdp) =>
        val j1 = system_evolution.addJoint(currentState, dp.p1, os.currentState, osdp.p1)
        val j2 = system_evolution.addJoint(currentState, dp.p2, os.currentState, osdp.p2)
        setDocked(Some(DockData(os, List(j1, j2), dp, osdp)))
        os.setDocked(Some(DockData(this, List(j1, j2), osdp, dp)))
    }
  }
  def undock(): Unit = {
    dock_data.foreach {
      case DockData(os, joints, our_dp, other_ship_dp) =>
        joints.foreach(j => system_evolution.removeJoint(j))
        os.setDocked(None)
    }
    dock_data = None
  }
  def setDocked(d:Option[DockData]): Unit = {
    dock_data = d
  }

  def coord = if(pilotIsAlive) currentState.coord else ship_parts.headOption.map(_.coord).getOrElse(currentState.coord)

  def linearVelocity = if(pilotIsAlive) currentState.vel else ship_parts.headOption.map(_.vel).getOrElse(currentState.vel)

  def relativeLinearVelocity = {
    linearVelocity - insideSphereOfInfluenceOfCelestialBody(coord, mass, OrbitalKiller.currentPlanetStates).map(_._2.vel).getOrElse(DVec.zero)
  }

  def velocityStr:String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, OrbitalKiller.currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        s"${msecOrKmsec((linearVelocity - planet_state.vel).norma)} (${planet.name}), [b${msecOrKmsec(linearVelocity.norma)} (абсолютная)]"
      case None =>
        s"${msecOrKmsec(linearVelocity.norma)} (абсолютная)"
    }
  }

  def angularVelocity = if(pilotIsAlive) currentState.ang_vel else ship_parts.headOption.map(_.ang_vel).getOrElse(currentState.ang_vel)

  def rotation = if(pilotIsAlive) currentState.ang else  ship_parts.headOption.map(_.ang).getOrElse(currentState.ang)

  def currentReactiveForce(time:Long, bs:BodyState):DVec = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(DVec.dzero) {
      case (sum, e) => sum + e.force.rotateDeg(bs.ang)
    }
  }

  def currentReactiveForce(tacts:Long, bs:MutableBodyState):DVec = {
    engines.filter(e => e.active && tacts < e.stopMomentTacts).foldLeft(DVec.dzero) {
      case (sum, e) => sum + e.force.rotateDeg(bs.ang)
    }
  }

  def fuelConsumptionPerTact:Double = {
    engines.filter(e => e.active).foldLeft(0.0) {
      case (sum, e) => sum + e.fuelConsumptionPerTact
    }
  }

  def fuelMassWhenEnginesOff:Double = {
    fuelMass - engines.filter(e => e.active).map(e => e.workTimeTacts*e.fuelConsumptionPerTact).sum
  }

  def fuelMassWhenEnginesOffWithoutEngine(ee:Engine):Double = {
    fuelMass - engines.filter(e => e.active && e != ee).map(e => e.workTimeTacts*e.fuelConsumptionPerTact).sum
  }

  def currentMass(time:Long, bs:BodyState):Double = {
    mass - engines.filter(e => e.active).foldLeft(0.0) {
      case (sum, e) =>
        sum + e.fuelConsumptionPerTact * (math.min(time, e.stopMomentTacts) - (e.stopMomentTacts - e.workTimeTacts))
    }
  }

  def currentMass(time:Long):Double = {
    mass - engines.filter(e => e.active).foldLeft(0.0) {
      case (sum, e) =>
        sum + e.fuelConsumptionPerTact * (math.min(time, e.stopMomentTacts) - (e.stopMomentTacts - e.workTimeTacts))
    }
  }

  def currentTorque(time:Long, bs:BodyState):Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) {
      case (sum, e) => sum + e.torque
    }
  }

  def currentTorque(time:Long, bs:MutableBodyState):Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) {
      case (sum, e) => sum + e.torque
    }
  }

  def currentTorque(time:Long):Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) {
      case (sum, e) => sum + e.torque
    }
  }

  def deactivateAllEngines(): Unit = {
    engines.foreach(_.active = false)
  }

  def activateOnlyTheseEngines(engines_to_activate:Engine*) {
    //timeMultiplier = realtime
    engines_to_activate.foreach(_.active = true)
    engines.withFilter(e => e.active && !engines_to_activate.contains(e)).foreach(_.active = false)
  }

  def activateOnlyOneEngine(e:Engine) {
    activateOnlyTheseEngines(e)
  }

  def allEnginesInactive:Boolean = {
    engines.forall(!_.active)
  }

  def engineColor(e:Engine, in_shadow:Boolean):ScageColor = {
    if(pilot_is_dead || e.active) RED else if(in_shadow) DARK_GRAY else WHITE
  }

  def engineActiveSize(e:Engine, max_size:Double):Double = {
    max_size*e.power/e.max_power
  }

  protected def drawEngine(e:Engine, size:Double = 1) {
    val is_vertical = e.force_dir.x == 0
    val (center, width, height) = e.force_dir match {
      case DVec(0, -1) => (e.position + DVec(0, 0.25)*size, 1*size, 0.5*size)
      case DVec(0, 1)  => (e.position + DVec(0, -0.25)*size, 1*size, 0.5*size)
      case DVec(-1, 0) => (e.position + DVec(0.25, 0)*size,  0.5*size,  1*size)
      case DVec(1, 0)  => (e.position + DVec(-0.25, 0)*size,  0.5*size,  1*size)
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
    if(isSelectedEngine(e)) drawRectCentered(center, width*1.5, height*1.5, color = engineColor(e, in_shadow))
    if(e.active && e.power > 0) {
      if(is_vertical) {
        drawFilledRectCentered(center, engineActiveSize(e, width), height, color = engineColor(e, in_shadow))
      } else {
        drawFilledRectCentered(center, width, engineActiveSize(e, height), color = engineColor(e, in_shadow))
      }
    }
    //print(s"${e.index}", e.position.toVec, color = WHITE, size = (max_font_size / globalScale).toFloat)
  }

  protected def drawDashedLine(from:DVec, to:DVec, dash_len:Double, color:ScageColor): Unit = {
    val line_len = (to - from).norma
    val normal = (to - from).n
    (0.0 to line_len-dash_len by dash_len*2).foreach {
      case dash_from => drawLine(from + normal*dash_from, from+normal*(dash_from + dash_len), color)
    }
  }

  protected def drawShip(): Unit = {
    if(!drawMapMode && coord.dist2(ship.coord) < 100000*100000) {
      if(pilotIsAlive) {
        openglLocalTransform {
          openglMove(coord - base)
          drawFilledCircle(DVec.zero, 2, GREEN) // mass center
          if (OrbitalKiller.globalScale >= 0.8) {
            drawArrow(DVec.zero, relativeLinearVelocity.n * 100, CYAN) // current velocity
          }

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
                val (p1_on_the_right_way, p2_on_the_right_way) = OrbitalKiller.ship.docking_points.headOption.map(_.pointsOnTheRightWay(dp)).getOrElse((false, false))

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
            case e => drawEngine(e, 10)
          }
        }
      } else {
        if(shipIsCrashed) {
          ship_parts.foreach(mbs => {
            val mbs_points = mbs.shape.asInstanceOf[PolygonShape].points
            openglLocalTransform {
              openglMove(mbs.coord - base)
              drawFilledCircle(DVec.zero, 0.3, GREEN)
              /*mbs.contacts.foreach(x => {
              if(x.a.index.contains("part") && x.b.index.contains("part")) {
                drawFilledCircle(x.contact_point - mbs.coord, 0.3, YELLOW)
                drawLine(x.contact_point - mbs.coord, x.contact_point - mbs.coord + x.normal.n, YELLOW)
                drawCircle(x.contact_point - mbs.coord, x.separation, YELLOW)
              }
            })*/
              openglRotateDeg(mbs.ang)
              drawSlidingLines(mbs_points :+ mbs_points.head, colorIfPlayerAliveOrRed(WHITE))
            }
          })
        } else {
          openglLocalTransform {
            openglMove(coord - base)
            openglRotateDeg(rotation)
            drawSlidingLines(draw_points, colorIfPlayerAliveOrRed(WHITE))
          }
        }
      }
    }
  }

  render {
    drawShip()
  }

  def preserveAngularVelocity(ang_vel_deg:Double)
  def preserveVelocity(vel:DVec)

  /**
   * rotation и angle_deg оба в диапазоне 0 - 360
   * @param angle_deg - угол между ориентацией корабля и вектором DVec(0, 1), который необходимо поддерживать
   */
  def preserveAngle(angle_deg:Double) {
    if(rotation != angle_deg) {
      if(rotation > angle_deg) {
        if(rotation - angle_deg < angle_deg - rotation + 360) {
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
      } else if(rotation < angle_deg) {
        if(rotation - angle_deg > angle_deg - rotation - 360) {
          val diff = rotation - angle_deg
          if(diff < -50) preserveAngularVelocity(10)
          else if(diff < -10) preserveAngularVelocity(5)
          else if(diff < -1) preserveAngularVelocity(1.8)
          else if(diff < -0.1) preserveAngularVelocity(0.2)
          else preserveAngularVelocity(0)
        } else {
          val diff = angle_deg - rotation - 360
          if(diff < -50) preserveAngularVelocity(-10)
          else if(diff < -10) preserveAngularVelocity(-5)
          else if(diff < -1) preserveAngularVelocity(-1.8)
          else if(diff < -0.1) preserveAngularVelocity(-0.2)
          else preserveAngularVelocity(0)
        }
      }
    }
  }

  protected var last_correction_or_check_moment:Long = 0l

  private var prev_flight_mode_and_engine_states:Option[(FlightMode, List[(Long, Double, Boolean)])] = None
  def haveSavedFlightMode = prev_flight_mode_and_engine_states.nonEmpty
  def saveFlightModeAndEngineStates(prev_flight_mode:FlightMode): Unit = {
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

  private var flight_mode:FlightMode = FreeFlightMode
  def flightMode:FlightMode = flight_mode
  def flightMode_=(new_flight_mode:FlightMode) {
    val prev_flight_mode = flight_mode
    flight_mode = new_flight_mode
    last_correction_or_check_moment = 0l
    if(flight_mode == Maneuvering) {
      val ten_min_or_max_time_at_full_power = math.min((fuelMass/engines.map(_.maxFuelConsumptionPerTact).max).toLong, 37800)
      if(InterfaceHolder.dockingSwitcher.dockingEnabled) {
        engines.foreach(e => e.power = 10000)
      } else {
        engines.foreach(e => e.power = {
          if(InterfaceHolder.gSwitcher.maxGSet) {
            math.min(
              mass * InterfaceHolder.gSwitcher.maxG * OrbitalKiller.earth.g + earth.airResistance(currentState, earth.currentState, 28, 0.5).norma,
              e.max_power * 0.5)
          } else {
            e.max_power * 0.5
          }
        })
      }
      engines.filterNot(_.active).foreach(e => e.workTimeTacts = ten_min_or_max_time_at_full_power)     // 10 minutes in tacts (10*60*63)
      val active_engines = engines.filter(_.active)
      if(active_engines.map(ae => ae.fuelConsumptionPerTact*ten_min_or_max_time_at_full_power).sum <= fuelMass) {
        active_engines.foreach(e => e.workTimeTacts = ten_min_or_max_time_at_full_power)
      } else {
        val fuel_for_every_active_engine = fuelMass / active_engines.length
        active_engines.foreach(e => e.workTimeTacts = (fuel_for_every_active_engine/e.fuelConsumptionPerTact).toLong)
      }
    } else {
      if(flight_mode == FreeFlightMode) {
        engines.foreach(e => e.active = false)
      } else if(prev_flight_mode == Maneuvering &&
        (flight_mode == Killrot ||
         flight_mode == NearestShipVelocity ||
         flight_mode == NearestShipAligned ||
         flight_mode == VelocityAligned ||
         flight_mode == OppositeVelocityAligned)) {
        saveFlightModeAndEngineStates(prev_flight_mode)
      } else if(flight_mode == NearestPlanetVelocity) {
        vertical_speed_msec = 0
        horizontal_speed_msec = 0
      }
      if(prev_flight_mode == Maneuvering && flight_mode != Maneuvering) {
        engines.foreach(e => e.workTimeTacts = 0)
      }
    }
  }

  def vertical_speed_msec:Int = 0
  def vertical_speed_msec_=(x:Int) {}
  def horizontal_speed_msec:Int = 0
  def horizontal_speed_msec_=(x:Int) {}

  def flightModeStr:String = flight_mode match {
    case NearestPlanetVelocity => s"уравнять скорость с ближайшей планетой: ${msecOrKmsec(vertical_speed_msec)}, ${msecOrKmsec(horizontal_speed_msec)}"
    case x => x.rusStr
  }

  def otherShipsNear:List[PolygonShip] = OrbitalKiller.ships.filter(s => s.index != index && s.pilotIsAlive).sortBy(s => coord.dist(s.coord))
  def canDockWithNearestShip:Boolean = {
    otherShipsNear.headOption.exists(os => {
      os.coord.dist(coord) < 1000 && docking_points.exists(dp => {
        os.docking_points.exists(osdp => dp.pointsMatch(osdp))
      })
    })
  }
  def possibleDockPointsWithNearestShip:List[(DockingPoints, PolygonShip, DockingPoints)] = {
    for {
      dp <- docking_points
      os <- otherShipsNear.headOption.toList
      osdp <- os.docking_points
      if dp.pointsMatch(osdp)
    } yield (dp, os, osdp)
  }

  def canDockWithNearestShipUsingDockPoints(dp:DockingPoints):Boolean = {
    otherShipsNear.headOption.toList.flatMap(_.docking_points).exists(osdp => dp.pointsMatch(osdp))
  }

  val pilot_mass = 75
  val pilot_position = DVec(0, 8)
  private var pilot_average_g:Double = 0.0
  private val pilot_accs = ArrayBuffer[(DVec, Long)]()

  private var pilot_is_dead = false
  private var pilot_death_reason = ""
  def pilotIsDead = pilot_is_dead
  def pilotIsAlive = !pilot_is_dead

  private var ship_is_crashed = false
  def shipIsCrashed = ship_is_crashed

  /**
   * Если на пилота действует перегрузка, уменьшается этот счетчик. Скорость его уменьшения зависит от величины перегрузки.
   * Если счетчик дойдет до нуля, пилот умирает. При отсутствии перегрузки (ускорение 1g или меньше), счетчик примерно за 30 секунд
   * возвращается до 100.
   */
  private var before_death_counter:Double = 100

  /**
   *
   * @param gs - перегрузка в единицах g
   * @return
   */
  private def deatchCounterDecreaseRate(gs:Double):Double = {
    def _linearFunc(p1:(Double, Double), p2:(Double, Double)):Double = {
      val a = (p1._2 - p2._2)/(p1._1 - p2._1)
      val b = p1._2 - a*p1._1
      val sec = a*gs + b  // за столько секунд счетчик дойдет от ста до нуля при данном значении перегрузки
      100.0/sec
    }

    // https://en.wikipedia.org/wiki/G-force#Human_tolerance_of_g-force
    // http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19980223621.pdf (p. 30, fig. 8)
    val ans = if(gs <= 4) 0
    else if(gs <= 6) {
      _linearFunc((4, 660), (6, 240))
    } else if(gs <= 8) {
      _linearFunc((6, 240), (8, 60))
    } else if(gs <= 10) {
      _linearFunc((8, 60), (10, 24))
    } else if(gs <= 11) {
      _linearFunc((10, 24), (11, 6))
    } else if(gs <= 20) {
      _linearFunc((11, 6), (20, 1))
    } else if(gs <= 27) {
      _linearFunc((20, 1), (27, 0.3))
    } else 100.0/0.01
    ans*base_dt
  }

  def colorIfPlayerAliveOrRed(color: => ScageColor) = if(OrbitalKiller.ship.pilotIsDead) RED else color

  protected var ship_parts:List[MutableBodyState] = Nil

  private def kill(reason:String, crash:Boolean): Unit = {
    pilot_is_dead = true
    pilot_death_reason = reason
    flightMode = FreeFlightMode
    engines.foreach(_.active = false)
    if(this.index == OrbitalKiller.ship.index) {
      viewMode = FixedOnShipAbsolute
    }
    if(isDocked) {
      undock()
    }
    if(crash) {
      OrbitalKiller.system_evolution.removeBodyByIndex(index)
      ship_parts = wreck_parts.zipWithIndex.map(x => {
        val part_center = currentState.coord + x._1.points.sum / x._1.points.length
        val part_points = x._1.points.map(p => currentState.coord + p - part_center)
        val part_index = ScageId.nextId
        val mbs = new MutableBodyState(BodyState(
          index = part_index,
          mass = mass / wreck_parts.length,
          vel = linearVelocity + randomSpeed(linearVelocity),
          coord = part_center,
          ang = rotation,
          shape = PolygonShape(part_points, Nil),
          is_static = false,
          restitution = 0.8
        ))
        OrbitalKiller.system_evolution.addBody(mbs,
          (tacts, helper) => {
            helper.gravityForceFromTo(sun.index, part_index) +
              helper.gravityForceFromTo(earth.index, part_index) +
              helper.gravityForceFromTo(moon.index, part_index) +
              helper.funcOfArrayOrDVecZero(Array(part_index, earth.index), l => {
                val bs = l(0)
                val e = l(1)
                earth.airResistance(bs, e, 28, 0.5)
              })
          },
          (tacts, helper) => 0.0
        )
        mbs
      })
      ship_is_crashed = true
    }
  }

  private def randomSpeed(vel:DVec) = {
    val dir_deg = 140.0 + math.random*80.0
    vel.n.rotateDeg(dir_deg)*50.0
  }

  def updateShipState(time_msec:Long): Unit = {
    val reactive_force = currentReactiveForce(0, currentState) + earth.airResistance(currentState, earth.currentState, 28, 0.5)
    if(!ship_is_crashed) {
      val dvel = currentState.dvel.norma
      if (dvel > 10) {
        // crash tolerance = 10 m/s
        val crash_g = dvel / OrbitalKiller.base_dt / earth.g
        kill(f"Корабль уничтожен в результате столкновения ($crash_g%.2fg)", crash = true)
        return
      } else {
        // ниже мы рассчитаем отдельно вертикальную и горизонтальную перегрузки и потом сложим их. Так надо считать, потому что к вертикальной перегрузке прибавляется центробежная сила, а к горизонтальной нет.
        val v_vert = pilot_position.rotateDeg(rotation).n  // единичный 22вектор спина-грудь пилота
        val v_hor = -v_vert.perpendicular // единичный вектор левая рука - права рука пилота
        val centrifugial_force = if (angularVelocity == 0) 0.0 else pilot_mass * math.pow(angularVelocity.toRad, 2) * pilot_position.norma
        val pilot_acc_vert = reactive_force / mass * v_vert + centrifugial_force / pilot_mass + currentState.dacc*v_vert
        val pilot_acc_hor = reactive_force / mass * v_hor + currentState.dacc*v_hor
        val pilot_acc = pilot_acc_vert*DVec(0, 1) + pilot_acc_hor*DVec(1,0) // тут мы умножаем на единичные векторы в системе координат: начало в центре масс, вертикальный вектор - от центра масс к пилоту
        pilot_accs += ((pilot_acc, time_msec))
        if (time_msec - pilot_accs.head._2 >= 1000) {
          pilot_average_g = (pilot_accs.map(_._1).sum / pilot_accs.length).norma / earth.g
          pilot_accs.clear()
          if (pilot_average_g > 100) {
            kill(f"Корабль разрушился вследствие критической перегрузки ($pilot_average_g%.2fg)", crash = true)
            return
          }
        }
      }
      currentPlanetStates.find {
        case (planet, planet_state) => planet.coord.dist(currentState.coord) < planet.radius
      }.foreach {
        case (planet, planet_state) =>
          currentState.coord = currentState.coord + (currentState.coord - planet.coord).n * (planet.radius + radius - planet.coord.dist(currentState.coord))
          currentState.vel = planet.linearVelocity
          kill("Корабль врезался в планету", crash = true)
          return
      }
    }
    if(!pilot_is_dead) {
      if(pilot_average_g > 4) { // пилот может испытывать перегрузку больше 4g только ограниченный период времени, потом наступает смерть
        val rate = deatchCounterDecreaseRate(pilot_average_g)
        before_death_counter -= rate
        if(before_death_counter <= 0) {
          kill(f"Пилот умер от сильной перегрузки ($pilot_average_g%.2fg)", crash = false)
        } else {
          val time_before_death_msec = ((before_death_counter/rate)*base_dt*1000).toLong
          if(time_before_death_msec <= 1000) {      // 1 секунда до гибели от перегрузки
            if(engines.exists(_.active)) {          // если работают какие-то двигатели
              flightMode = FreeFlightMode           // отключаем двигатели
              engines.foreach(_.active = false)
            }
          }
        }
      } else if(before_death_counter != 100) {
        if(pilot_average_g <= 1.09) { // пилот может восстанавливаться после перегрузки, только если текущая ускорение не выше 1.09g
          val rate = 100.0/60*base_dt // восстановление после критической перегрузки за 60 секунд
          before_death_counter += rate
          if(before_death_counter >= 100) {
            before_death_counter = 100
          }
        }
      }
      if(InterfaceHolder.gSwitcher.maxGSet && pilot_average_g > InterfaceHolder.gSwitcher.maxG) {
        val active_engines = engines.filter(e => e.active && 0 < e.stopMomentTacts)
        val cur_force = reactive_force.norma
        val allowed_force = mass*InterfaceHolder.gSwitcher.maxG*OrbitalKiller.earth.g
        val force_diff = cur_force - allowed_force
        val force_diff_for_engine = force_diff/active_engines.length
        active_engines.foreach(e => if(force_diff_for_engine < e. power) {
          e.power -= force_diff_for_engine
        })

      }
    }
  }

  def pilotStateStr:String = {
    if(!pilot_is_dead) {
      if (pilot_average_g < 0.1) {
        if(before_death_counter != 100) {
          val rate = 100.0/60*base_dt // восстановление после критической перегрузки за 60 секунд
          val time_to_restore_msec = (((100 - before_death_counter)/rate)*base_dt*1000).toLong
          f"Пилот в состоянии невесомости. Восстанавливается после перегрузки (${timeStr(time_to_restore_msec)})"
        } else {
          s"Пилот в состоянии невесомости ($pilot_average_g)"
        }
      } else if (pilot_average_g <= 1.09) {
        if(before_death_counter != 100) {
          val rate = 100.0/60*base_dt // восстановление после критической перегрузки за 60 секунд
          val time_to_restore_msec = (((100 - before_death_counter)/rate)*base_dt*1000).toLong
          f"Пилот испытывает силу тяжести $pilot_average_g%.1fg. Восстанавливается после перегрузки (${timeStr(time_to_restore_msec)})"
        } else {
          f"Пилот испытывает силу тяжести $pilot_average_g%.1fg"
        }
      } else {
        if(pilot_average_g > 4) {
          val rate = deatchCounterDecreaseRate(pilot_average_g)
          val time_before_death_msec = ((before_death_counter/rate)*base_dt*1000).toLong
          f"[rПилот испытывает критическую перегрузку $pilot_average_g%.1fg. Смерть через ${timeStr(time_before_death_msec)}]"
        } else {
          if(before_death_counter != 100) {
            f"Пилот испытывает перегрузку $pilot_average_g%.1fg. [oТребуется отдых после критической перегрузки]"
          } else {
            f"Пилот испытывает перегрузку $pilot_average_g%.1fg"
          }
        }
      }
    } else {
      pilot_death_reason
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
  def points:List[DVec]
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


  lazy val radius = {
    val x = points.map(_.x).max - points.map(_.x).min
    val y = points.map(_.y).max - points.map(_.y).min
    math.max(x,y)
  }

  def initState:BodyState = BodyState(
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

  lazy val currentState:MutableBodyState = initState.toMutableBodyState

  var orbitRender:Option[BodyOrbitRender] = None
}
