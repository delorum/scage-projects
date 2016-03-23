package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{ScageId, DVec}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

import scala.collection.mutable.ArrayBuffer

sealed trait FlightMode

case object Free                    extends FlightMode
case object Killrot                 extends FlightMode
case object AxisAligned             extends FlightMode
case object VelocityAligned         extends FlightMode
case object OppositeVelocityAligned extends FlightMode
case object CirclularOrbit          extends FlightMode
case object NearestShipVelocity     extends FlightMode
case object NearestPlanetVelocity   extends FlightMode
case object AbsoluteStop            extends FlightMode
case object Maneuvering             extends FlightMode

class DockingPoints(val p1:DVec, val p2:DVec, ship:PolygonShip) {
  def curP1 = ship.currentState.coord + p1.rotateDeg(ship.currentState.ang)
  def curP1vel = ship.currentState.vel + (ship.currentState.ang_vel*p1.rotateDeg(90))
  def curP2 = ship.currentState.coord + p2.rotateDeg(ship.currentState.ang)
  def curP2vel = ship.currentState.vel + (ship.currentState.ang_vel*p2.rotateDeg(90))
  def pointsMatch(other_ship_docking_points:DockingPoints):Boolean = {
    curP1.dist(other_ship_docking_points.curP1) < 1 && curP2.dist(other_ship_docking_points.curP2) < 1
  }
  private def _checkAllConditions(conditions:(() => Boolean)*):Boolean = {
    if(conditions.isEmpty) true
    else if(conditions.head()) _checkAllConditions(conditions.tail:_*)
    else false
  }
  
  def pointsOnTheRightWay(dp:DockingPoints):(Boolean, Boolean) = {
    val vv1 = (dp.curP1-dp.curP2).n
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
case class DockData(dock_to_ship:PolygonShip, joints:List[Joint])

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

  def convex_parts:List[PolygonShape] = Nil
  def wreck_parts:List[PolygonShape] = Nil
  def docking_points:List[DockingPoints] = Nil

  private var dock_data:Option[DockData] = None
  def isDocked:Boolean = dock_data.nonEmpty
  def notDocked:Boolean = dock_data.isEmpty
  def dock(): Unit = {
    dockPointsWithNearestShip.headOption.foreach {
      case (dp, os, osdp) =>
        val j1 = system_evolution.addJoint(currentState, dp.p1, os.currentState, osdp.p1)
        val j2 = system_evolution.addJoint(currentState, dp.p2, os.currentState, osdp.p2)
        setDocked(Some(DockData(os, List(j1, j2))))
        os.setDocked(Some(DockData(this, List(j1, j2))))
    }
  }
  def undock(): Unit = {
    dock_data.foreach {
      case DockData(os, joints) =>
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

  def currentReactiveForce(time:Long, bs:MutableBodyState):DVec = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(DVec.dzero) {
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

  def engineColor(e:Engine):ScageColor = {
    if(pilot_is_dead || e.active) RED else WHITE
  }

  def engineActiveSize(e:Engine, max_size:Double):Double = {
    max_size*e.power/e.max_power
  }

  def drawEngine(e:Engine, size:Double = 1) {
    val is_vertical = e.force_dir.x == 0
    val (center, width, height) = e.force_dir match {
      case DVec(0, -1) => (e.position + DVec(0, 0.25)*size, 1*size, 0.5*size)
      case DVec(0, 1)  => (e.position + DVec(0, -0.25)*size, 1*size, 0.5*size)
      case DVec(-1, 0) => (e.position + DVec(0.25, 0)*size,  0.5*size,  1*size)
      case DVec(1, 0)  => (e.position + DVec(-0.25, 0)*size,  0.5*size,  1*size)
      case _ => throw new Exception("engine force dir other than vertical or horizontal is not supported")
    }
    drawRectCentered(center, width, height, color = engineColor(e))
    if(isSelectedEngine(e)) drawRectCentered(center, width*1.5, height*1.5, color = engineColor(e))
    if(e.active && e.power > 0) {
      if(is_vertical) {
        drawFilledRectCentered(center, width, engineActiveSize(e, height), color = engineColor(e))
      } else {
        drawFilledRectCentered(center, engineActiveSize(e, width), height, color = engineColor(e))
      }
    }
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

  private var flight_mode:FlightMode = Free
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
        engines.foreach(e => e.power = e.max_power*0.5)
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
      if(flight_mode == Free) {
        engines.foreach(e => e.active = false)
      } else if(flight_mode == Killrot && prev_flight_mode == Maneuvering) {
        saveFlightModeAndEngineStates(prev_flight_mode)
      } else if(flight_mode == NearestShipVelocity && prev_flight_mode == Maneuvering) {
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
    case Free => "свободный"
    case Killrot => "запрет вращения"
    case AxisAligned => "ориентация по осям"
    case VelocityAligned => "ориентация по траектории"
    case OppositeVelocityAligned => "ориентация против траектории"
    case CirclularOrbit => "выход на круговую орбиту"
    case NearestShipVelocity => "уравнять скорость с кораблем"
    case NearestPlanetVelocity => s"уравнять скорость с ближайшей планетой: ${msecOrKmsec(vertical_speed_msec)}, ${msecOrKmsec(horizontal_speed_msec)}"
    case AbsoluteStop => "остановиться"
    case Maneuvering => "маневрирование"
    case _ => ""
  }

  def otherShipsNear:List[PolygonShip] = OrbitalKiller.ships.filter(s => s.index != index && s.pilotIsAlive).sortBy(s => coord.dist(s.coord))
  def canDockWithNearestShip:Boolean = {
    docking_points.exists(dp => {
      otherShipsNear.headOption.toList.flatMap(_.docking_points).exists(osdp => dp.pointsMatch(osdp))
    })
  }
  def dockPointsWithNearestShip:List[(DockingPoints, PolygonShip, DockingPoints)] = {
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

  protected val pilot_mass = 75
  protected val pilot_position = DVec(0, 8)
  private var pilot_average_g:Double = 0.0
  private val pilot_accs = ArrayBuffer[(DVec, Long)]()

  private var pilot_is_dead = false
  private var pilot_death_reason = ""
  def pilotIsDead = pilot_is_dead
  def pilotIsAlive = !pilot_is_dead

  def colorIfAliveOrRed(color: => ScageColor) = if(pilot_is_dead) RED else color

  protected var ship_parts:List[MutableBodyState] = Nil

  private def crash(reason:String): Unit = {
    pilot_is_dead = true
    pilot_death_reason = reason
    if(this.index == OrbitalKiller.ship.index) {
      flightMode = Free
      viewMode = 2
    }
    OrbitalKiller.system_evolution.removeBodyByIndex(index)
    if(isDocked) {
      undock()
    }
    ship_parts = wreck_parts.zipWithIndex.map(x => {
      val part_center = currentState.coord + x._1.points.sum/x._1.points.length
      val part_points = x._1.points.map(p => currentState.coord + p - part_center)
      val part_index = ScageId.nextId
      val mbs = new MutableBodyState(BodyState(
        index = part_index,
        mass = mass/wreck_parts.length,
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
  }

  private def randomSpeed(vel:DVec) = {
    val dir_deg = 140.0 + math.random*80.0
    vel.n.rotateDeg(dir_deg)*50.0
  }

  def updateShipState(time_msec:Long): Unit = {
    if(!pilot_is_dead) {
      val dvel = currentState.dvel.norma
      if (dvel > 10) {
        // crash tolerance = 10 m/s
        val crash_g = dvel / OrbitalKiller.base_dt / earth.g
        crash(f"Корабль уничтожен в результате столкновения ($crash_g%.2fg)")
        return
      } else {
        val reactive_force = currentReactiveForce(0, currentState) + earth.airResistance(currentState, earth.currentState, 28, 0.5)
        val centrifugial_force = if (angularVelocity == 0) DVec.zero else pilot_mass * math.pow(angularVelocity.toRad, 2) * pilot_position.rotateDeg(rotation)
        val pilot_acc = reactive_force / mass + centrifugial_force / pilot_mass + currentState.dacc
        pilot_accs += ((pilot_acc, time_msec))
        if (time_msec - pilot_accs.head._2 >= 1000) {
          pilot_average_g = (pilot_accs.map(_._1).sum / pilot_accs.length).norma / earth.g
          pilot_accs.clear()
          if (pilot_average_g > 100) {
            crash(f"Корабль разрушился вследствие критической перегрузки ($pilot_average_g%.2fg)")
            return
          }
        }
      }
      currentPlanetStates.find {
        case (planet, planet_state) => planet.coord.dist(currentState.coord) < planet.radius
      }.foreach {
        case (planet, planet_state) =>
          currentState.coord = currentState.coord + (currentState.coord - planet.coord).n*(planet.radius + radius - planet.coord.dist(currentState.coord))
          currentState.vel = planet.linearVelocity
          crash("Корабль врезался в планету")
          return
      }
    }
  }

  def pilotStateStr:String = {
    if(!pilot_is_dead) {
      if (pilot_average_g < 0.1) "Пилот в состоянии невесомости"
      else if (pilot_average_g <= 1) f"Пилот испытывает силу тяжести $pilot_average_g%.1fg"
      else f"Пилот испытывает перегрузку $pilot_average_g%.1fg"
    } else {
      pilot_death_reason
    }
  }

  def massStr = f"Масса корабля: ${gOrKg(mass)}. Остаток топлива: ${gOrKg(fuelMass)}"

  def shadowSideStr = {
    val in_shadow_of_planet:Option[String] = {
      val ship_sun_dist = coord.dist(sun.coord)
      currentPlanetStates.filterNot(_._1.index == sun.index).find {
        case (planet, planet_state) =>
          ship_sun_dist > planet.coord.dist(sun.coord) && (tangentsFromCircleToCircle(planet.coord, planet.radius, sun.coord, sun.radius) match {
            case Some((c1, c2, b1, b2)) =>
              val a1 = (c1 - b1).perpendicular * (coord - b1) > 0
              val a2 = (c2 - b2).perpendicular * (coord - b2) < 0
              a1 && a2
            case None => false
          })
      }.map(_._1.name)
    }
    in_shadow_of_planet match {
      case Some(planet_name) =>
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

  def points:List[DVec]
  def draw_points:List[DVec]

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
}
