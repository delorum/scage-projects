package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller.interface.elements.OtherShipInfo
import com.github.dunnololda.scageprojects.orbitalkiller.ships.ProxyShip
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ErrorConstants.angular_velocity_error
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ObjectIndices.planetIndices
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.PlanetWithAir
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interfaces.InterfaceHolderAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder.ShipsHolderAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.PolygonShape
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.{BodyState, MutableBodyState}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.render.orbits.OrbitRenderData
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.MyDouble
import com.github.dunnololda.scageprojects.orbitalkiller_cake.{Main, TimeConstants}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait FlightMode {
  def rusStr: String
}

// 1
case object FreeFlightMode extends FlightMode {
  override def rusStr: String = "свободный"
}

// 2
case object Killrot extends FlightMode {
  override def rusStr: String = "запрет вращения"
}

// 3
case object RelativeVelocityAligned extends FlightMode {
  override def rusStr: String = "ориентация по траектории"
}

// shift+3
case object OppositeRelativeVelocityAligned extends FlightMode {
  override def rusStr: String = "ориентация против траектории"
}

// 4
case object CirclularOrbit extends FlightMode {
  override def rusStr: String = "выход на круговую орбиту"
}

// 5
case object NearestShipVelocity extends FlightMode {
  override def rusStr: String = "уравнять скорость с кораблем"
}

// 6
case object NearestShipAligned extends FlightMode {
  override def rusStr: String = "ориентация на корабль"
}

// 7
case object NearestShipAutoDocking extends FlightMode {
  override def rusStr: String = "стыковка с кораблем"
}

// 8
case object NearestPlanetVelocity extends FlightMode {
  override def rusStr: String = "уравнять скорость с ближайшей планетой"
}

// 9
// vacant

// 0
case object Maneuvering extends FlightMode {
  override def rusStr: String = "маневрирование"
}

case class DisabledEngine(e: Int, instead_engines: List[Int] = Nil)

class DockingPoints(
    val p1: DVec,
    val p2: DVec,
    ship: PolygonShip,
    val disabled_engine: Option[DisabledEngine],
    val ordered_hull: List[DVec]) {
  val index: Int = ScageId.nextId
  private val joint_point: DVec = p1 + (p2 - p1) * 0.5
  val dock_dir: DVec = joint_point.n
  val dock_dist = 0.5

  // в метрах, при каком расстоянии между точками стыковки двух кораблей происходит захватю Для простоты это значение - одинаковая для всех константа. Вынесли сюда, чтобы было одно место, где поменять.
  def curP1: DVec = ship.currentState.coord + p1.rotateDeg(ship.currentState.ang)

  def curP1vel: DVec = ship.currentState.vel + (ship.currentState.ang_vel * p1.rotateDeg(90))

  def curP2: DVec = ship.currentState.coord + p2.rotateDeg(ship.currentState.ang)

  def curP2vel: DVec = ship.currentState.vel + (ship.currentState.ang_vel * p2.rotateDeg(90))

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

    val p1_on_the_right_way =
      (curP1 - (dp.curP1 + vv1)).perpendicular * vv2 < 0 && (curP1 - (dp.curP1 - vv1)).perpendicular * vv2 > 0 // p1 inside line
    val p2_on_the_right_way =
      (curP2 - (dp.curP2 + vv1)).perpendicular * vv2 < 0 && (curP2 - (dp.curP2 - vv1)).perpendicular * vv2 > 0 // p2_inside_line
    (p1_on_the_right_way, p2_on_the_right_way)
  }
}

case class DockData(
    dock_to_ship: PolygonShip,
    our_dp: DockingPoints,
    other_ship_dp: DockingPoints,
    proxy_ship: ProxyShip)

abstract class PolygonShip(
    val index: Int,
    val name: String,
    protected val init_coord: DVec,
    protected val init_velocity: DVec = DVec.dzero,
    protected val init_rotation: Double = 0,
    ship_designer: Boolean,
    create_interface: Boolean)
  extends ShipsHolderAware
  with InterfaceHolderAware {
  println(s"$name -> $index")
  protected var selected_engine: Option[Engine] = None

  def selectedEngine: List[Engine] = selected_engine match {
    case Some(e) =>
      if (!engineDisabled(e.index)) {
        List(e)
      } else {
        insteadDisabledEngine(e.index).flatMap(ide => engines.find(_.index == ide))
      }
    case None =>
      Nil
  }

  def clearEngineSelection(): Unit = {
    selected_engine = None
  }

  def isSelectedEngine(e: Engine): Boolean = {
    selected_engine.exists(x => {
      if (!engineDisabled(x.index)) {
        e.index == x.index
      } else {
        insteadDisabledEngine(x.index).contains(e.index)
      }
    })
  }

  def engines: List[Engine]
  def engines_by_keycodes: Map[Int, Engine]

  /*def switchEngineActive(engine_code:Int) {
    //timeMultiplier = realtime
    engines_mapping.get(engine_code).foreach(e => e.switchActive())
  }*/

  def selectOrSwitchEngineActive(key_code: Int): Unit = {
    engines_by_keycodes
      .get(key_code)
      .foreach(e => {
        selected_engine match {
          case Some(se) =>
            if (e.index == se.index) {
              val selected = selectedEngine
              if (selected.length == 1) {
                selected.head.switchActive()
              } else {
                selected.head.switchActive()
                selected.tail.foreach(x => {
                  x.active = selected.head.active
                  x.powerPercent = selected.head.powerPercent
                })
              }
            } else {
              selected_engine = Some(e)
              val selected = selectedEngine
              if (selected.length > 1) {
                selected.tail.foreach(x => {
                  x.powerPercent = selected.head.powerPercent
                })
              }
            }
          case None =>
            selected_engine = Some(e)
            val selected = selectedEngine
            if (selected.length > 1) {
              selected.tail.foreach(x => {
                x.powerPercent = selected.head.powerPercent
              })
            }
        }
      })
  }

  def mass: Double

  def thisOrActualProxyShipMass: Double = dock_data.map(_.proxy_ship.thisOrActualProxyShipMass).getOrElse(mass)

  def thisOrActualProxyShipI: Double = dock_data.map(_.proxy_ship.thisOrActualProxyShipI).getOrElse(currentState.I)

  def fuelMass: Double

  def fuelMass_=(m: Double): Unit

  def engine_size: Double

  def convex_parts: List[PolygonShape]

  def wreck_parts: List[PolygonShape]

  def docking_points: List[DockingPoints]

  def createOrderedHull(order: List[(Int, Int)]): List[DVec] = order.flatMap { case (from, to) =>
    points.slice(from - 1, from - 1 + to)
  }

  protected var dock_data: Option[DockData] = None

  def dockData: Option[DockData] = dock_data

  def isDocked: Boolean = dock_data.nonEmpty

  def isDockedToShip(other_ship: PolygonShip): Boolean =
    thisOrActualProxyShipIndex == other_ship.thisOrActualProxyShipIndex

  def notDocked: Boolean = dock_data.isEmpty

  private val standard_angles = List(0, 90, -90, 180, -180, 270, -270, 360, -360)

  def dock(): Unit = {
    possibleDockPointsWithNearestShip.headOption.foreach { case (dp, os, osdp) =>
      val ship = thisOrActualProxyShip
      val rotation_diff = rotation - os.rotation
      standard_angles
        .find(angle => rotation_diff.equalPlusMinusTen(angle))
        .foreach(angle => {
          ship.currentState.ang = os.rotation + angle
        })
      val correction = osdp.curP1 - dp.curP1
      ship.currentState.coord += correction
      val proxy_ship = new ProxyShip(ship, ship.coord, ship.rotation, dp, os, os.coord, os.rotation, osdp)
        with ShipsHolderAwareImpl
        with InterfaceHolderAwareImpl
      ship.currentState.active = false
      os.currentState.active = false
      ship.setDocked(Some(DockData(os, dp, osdp, proxy_ship)))
      os.setDocked(Some(DockData(ship, osdp, dp, proxy_ship)))
      ship.ship_interface.foreach(_.forceUpdate())
      os.ship_interface.foreach(_.forceUpdate())
    }
  }

  def undock(): Unit = {
    dock_data.foreach { case DockData(os, _, _, proxy_ship) =>
      proxy_ship.updateShipState(index)
      proxy_ship.updateShipState(os.index)
      currentState.active = true
      os.currentState.active = true
      shipsHolder.removeShipByIndex(proxy_ship.index)
      os.setDocked(None)
    }
    dock_data = None
  }

  def setDocked(d: Option[DockData]): Unit = {
    dock_data = d
  }

  def nearestFreeDockingPoints(coord: DVec): Option[DockingPoints] = {
    docking_points.filter(dp => !dock_data.exists(_.our_dp.index == dp.index)).sortBy(_.curP1.dist(coord)).headOption
  }

  /**
   * Возвращает либо индекс данного корабля, либо, если он пристыкован - индекс proxy-корабля, либо если и тот пристыкован - итд
   */
  def thisOrActualProxyShipIndex: Int = dock_data.map(_.proxy_ship.thisOrActualProxyShipIndex).getOrElse(index)

  def thisOrActualProxyShip: PolygonShip = dock_data.map(_.proxy_ship.thisOrActualProxyShip).getOrElse(this)

  implicit class DockCorrection(v: DVec) {

    def actualPosBeforeRotation: DVec = {
      dock_data match {
        case Some(dd) =>
          val (our_coord_diff, our_rotation_diff) = dd.proxy_ship.coordAndRotationDiff(index)
          if (our_rotation_diff != 0) {
            val proxy_ship_rotation = dd.proxy_ship.thisOrActualProxyShipRotation
            if (proxy_ship_rotation != 0) {
              (v.rotateDeg(our_rotation_diff) + our_coord_diff).rotateDeg(dd.proxy_ship.thisOrActualProxyShipRotation)
            } else {
              v.rotateDeg(our_rotation_diff) + our_coord_diff
            }
          } else {
            val proxy_ship_rotation = dd.proxy_ship.thisOrActualProxyShipRotation
            if (proxy_ship_rotation != 0) {
              (v + our_coord_diff).rotateDeg(dd.proxy_ship.thisOrActualProxyShipRotation)
            } else {
              v + our_coord_diff
            }
          }
        case None =>
          v
      }
    }

    /**
     * Если мы пристыкованы, то центр масс другой, и мб мы пристыкованы под углом, то есть все координаты, которые были относительно
     * нашего центра масс при условии вертикальной ориентации надо пересчитать
     */
    def actualPos: DVec = {
      dock_data match {
        case Some(dd) =>
          val (our_coord_diff, our_rotation_diff) = dd.proxy_ship.coordAndRotationDiff(index)
          if (our_rotation_diff != 0) {
            v.rotateDeg(our_rotation_diff) + our_coord_diff
          } else {
            v + our_coord_diff
          }
        case None =>
          v
      }
    }

    def actualDir: DVec = {
      dock_data match {
        case Some(dd) =>
          val (_, our_rotation_diff) = dd.proxy_ship.coordAndRotationDiff(index)
          if (our_rotation_diff != 0) {
            v.rotateDeg(our_rotation_diff)
          } else {
            v
          }
        case None =>
          v
      }
    }
  }

  def ourCoordAndRotationDiff: (DVec, Double) = {
    dock_data match {
      case Some(dd) =>
        dd.proxy_ship.coordAndRotationDiff(index)
      case None =>
        (DVec.zero, 0.0)
    }
  }

  def isLanded: Boolean = {
    _orbit_data.exists(_.is_landed)
  }

  def isLandedOnEarth: Boolean = {
    _orbit_data.exists(_.is_landed_on_earth)
  }

  def isLandedOnMoon: Boolean = {
    _orbit_data.exists(_.is_landed_on_moon)
  }

  def coord: DVec = if (isAlive) currentState.coord else main_ship_wreck.map(_.coord).getOrElse(currentState.coord)

  def linearVelocity: DVec =
    if (isAlive) currentState.vel else main_ship_wreck.map(_.linearVelocity).getOrElse(currentState.vel)

  def relativeLinearVelocity: DVec = {
    linearVelocity - _orbit_data.map(_.planet_state.vel).getOrElse(DVec.zero)
  }

  private def velocityStr: String = {
    _orbit_data match {
      case Some(or) =>
        or.planet match {
          case air_planet: PlanetWithAir =>
            if (air_planet.altitude(coord, air_planet.coord) < air_planet.air_free_altitude) {
              val vel1 = (linearVelocity - or.planet_state.vel).norma
              val vel2 = air_planet
                .velocityRelativeToAir(
                  coord,
                  linearVelocity,
                  air_planet.coord,
                  air_planet.linearVelocity,
                  air_planet.init_ang_vel
                )
                .norma
              val atmo_efficiency = air_planet
                .terminalVelocity(mass, coord, air_planet.coord, 28, 0.5)
                .map(tvel => vel2 / tvel * 100)
                .getOrElse(100.0)
              f"${msecOrKmsecOrKmhour(vel1)} $atmo_efficiency%.2f%% (${or.planet.name}), [b${msecOrKmsecOrKmhour(linearVelocity.norma)} (абсолютная)]"
            } else {
              s"${msecOrKmsecOrKmhour((linearVelocity - or.planet_state.vel).norma)} (${or.planet.name}), [b${msecOrKmsecOrKmhour(linearVelocity.norma)} (абсолютная)]"
            }
          case _ =>
            s"${msecOrKmsecOrKmhour((linearVelocity - or.planet_state.vel).norma)} (${or.planet.name}), [b${msecOrKmsecOrKmhour(linearVelocity.norma)} (абсолютная)]"
        }
      case None =>
        s"${msecOrKmsecOrKmhour(linearVelocity.norma)} (абсолютная)"
    }
  }

  def thisOrActualProxyShipVelocityStr: String =
    dock_data.map(_.proxy_ship.thisOrActualProxyShipVelocityStr).getOrElse(velocityStr)

  def angularVelocity: Double = if (isAlive) currentState.ang_vel
  else main_ship_wreck.map(_.angularVelocity).getOrElse(currentState.ang_vel)

  def thisOrActualProxyShipAngularVelocity: Double =
    dock_data.map(_.proxy_ship.thisOrActualProxyShipAngularVelocity).getOrElse(angularVelocity)

  def rotation: Double =
    if (isAlive) currentState.ang else main_ship_wreck.map(_.rotation).getOrElse(currentState.ang)

  def thisOrActualProxyShipRotation: Double = {
    dock_data.map(_.proxy_ship.thisOrActualProxyShipRotation).getOrElse(rotation)
  }

  def currentReactiveForce(time: Long, bs: BodyState): DVec = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(DVec.dzero) { case (sum, e) =>
      sum + (e.force_dir.actualDir * e.power).rotateDeg(bs.ang)
    }
  }

  def currentReactiveForce(tacts: Long, bs: MutableBodyState): DVec = {
    engines.filter(e => e.active && tacts < e.stopMomentTacts).foldLeft(DVec.dzero) { case (sum, e) =>
      sum + (e.force_dir.actualDir * e.power).rotateDeg(bs.ang)
    }
  }

  def currentMass(time: Long): Double = {
    mass - engines.filter(e => e.active).foldLeft(0.0) { case (sum, e) =>
      sum + e.fuelConsumptionPerTact * (math.min(time, e.stopMomentTacts) - (e.stopMomentTacts - e.workTimeTacts))
    }
  }

  def thisOrActualProxyShipCurrentMass(time: Long): Double = {
    dock_data.map(_.proxy_ship.thisOrActualProxyShipCurrentMass(time)).getOrElse(currentMass(time))
  }

  def currentTorque(time: Long): Double = {
    engines.filter(e => e.active && time < e.stopMomentTacts).foldLeft(0.0) { case (sum, e) =>
      val e_torque = -(e.force_dir.actualDir * e.power) */ e.position.actualPos
      sum + e_torque
    }
  }

  def fuelConsumptionPerTact: Double = {
    engines.filter(e => e.active).foldLeft(0.0) { case (sum, e) =>
      sum + e.fuelConsumptionPerTact
    }
  }

  def fuelMassWhenEnginesOff: Double = {
    fuelMass - engines.filter(e => e.active).map(e => e.workTimeTacts * e.fuelConsumptionPerTact).sum
  }

  def fuelMassWhenEnginesOffWithoutEngine(ee: Engine): Double = {
    fuelMass - engines.filter(e => e.active && e != ee).map(e => e.workTimeTacts * e.fuelConsumptionPerTact).sum
  }

  def deactivateAllEngines(): Unit = {
    engines.foreach(_.active = false)
  }

  def activateOnlyTheseEngines(engines_to_activate: Engine*): Unit = {
    // timeMultiplier = realtime
    engines_to_activate.foreach(_.active = true)
    engines.withFilter(e => e.active && !engines_to_activate.contains(e)).foreach(_.active = false)
  }

  def activateOnlyOneEngine(e: Engine): Unit = {
    activateOnlyTheseEngines(e)
  }

  def allEnginesInactive: Boolean = {
    engines.forall(!_.active)
  }

  private def engineColor(e: Engine, in_shadow: Boolean): ScageColor = {
    if (is_dead || e.active) RED else if (in_shadow) DARK_GRAY else WHITE
  }

  private def engineActiveSize(e: Engine, max_size: Double): Double = {
    max_size * e.power / e.max_power
  }

  def engineDisabled(engine_index: Int): Boolean = {
    dock_data match {
      case Some(dd) =>
        dd.our_dp.disabled_engine.exists(_.e == engine_index) || dd.proxy_ship.engineDisabled(engine_index)
      case None =>
        false
    }
  }

  def insteadDisabledEngine(engine_index: Int): List[Int] = {
    dock_data match {
      case Some(dd) =>
        dd.our_dp.disabled_engine match {
          case Some(de) =>
            if (de.e == engine_index) {
              de.instead_engines
            } else {
              dd.proxy_ship.insteadDisabledEngine(engine_index)
            }
          case None =>
            dd.proxy_ship.insteadDisabledEngine(engine_index)
        }
      case None =>
        Nil
    }
  }

  def drawEngine(e: Engine): Unit = {
    if (!engineDisabled(e.index)) {
      val (coord_diff, rotation_diff) = ourCoordAndRotationDiff
      val force_dir = {
        if (rotation_diff.equalPlusMinusOne(-90)) -e.force_dir.perpendicular
        else if (rotation_diff.equalPlusMinusOne(270)) -e.force_dir.perpendicular
        else if (rotation_diff.equalPlusMinusOne(90)) e.force_dir.perpendicular
        else if (rotation_diff.equalPlusMinusOne(-270)) e.force_dir.perpendicular
        else if (rotation_diff.equalPlusMinusOne(180)) e.force_dir * -1
        else if (rotation_diff.equalPlusMinusOne(-180)) e.force_dir * -1
        else e.force_dir
      }
      val is_vertical = force_dir.x == 0
      val (center, width, height) = force_dir match {
        case DVec(0, -1) =>
          (
            (e.position.rotateDeg(rotation_diff) + DVec(0, 0.25) * engine_size) + coord_diff,
            1 * engine_size,
            0.5 * engine_size
          )
        case DVec(0, 1) =>
          (
            (e.position.rotateDeg(rotation_diff) + DVec(0, -0.25) * engine_size) + coord_diff,
            1 * engine_size,
            0.5 * engine_size
          )
        case DVec(-1, 0) =>
          (
            (e.position.rotateDeg(rotation_diff) + DVec(0.25, 0) * engine_size) + coord_diff,
            0.5 * engine_size,
            1 * engine_size
          )
        case DVec(1, 0) =>
          (
            (e.position.rotateDeg(rotation_diff) + DVec(-0.25, 0) * engine_size) + coord_diff,
            0.5 * engine_size,
            1 * engine_size
          )
        case _ =>
          println(s"${e.ship.name} ${e.name} ${e.force_dir} $rotation_diff $force_dir")
          throw new Exception("engine force dir other than vertical or horizontal is not supported")
      }
      if (globalScale >= 20 && interfaceHolder.namesSwitcher.showNames) {
        print(e.name, e.position.actualPos.toVec, (max_font_size / globalScale).toFloat, WHITE)
        drawArrow(center, center + force_dir * radius / 6, WHITE)
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
      // print(s"${e.index}", e.position.toVec, color = WHITE, size = (max_font_size / globalScale).toFloat)
    }
  }

  def drawIfAliveBeforeRotation(): Unit = {
    /*drawFilledCircle(DVec.zero, 2, GREEN) // mass center
          if (Main.globalScale >= 0.8) {
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
  }

  def drawIfAliveAfterRotation(): Unit = {
    drawSlidingLines(actualDrawPoints, WHITE)
    if (Main.globalScale >= 0.8) {
      dock_data.foreach(d => {
        drawFilledCircle(d.our_dp.p1.actualPos, 0.3, colorIfPlayerAliveOrRed(GREEN))
        drawFilledCircle(d.our_dp.p2.actualPos, 0.3, colorIfPlayerAliveOrRed(GREEN))
      })
      if (interfaceHolder.dockingSwitcher.dockingEnabled && ship_interface.exists(!_.isMinimized)) {
        shipCloser2Km.foreach(s =>
          nearestFreeDockingPoints(s.coord).foreach(dp => {
            val (p1_on_the_right_way, p2_on_the_right_way) = {
              shipCloser2Km
                .flatMap(s => s.nearestFreeDockingPoints(coord).map(_.pointsOnTheRightWay(dp)))
                .getOrElse((false, false))
            }

            val c1 = if (p1_on_the_right_way) GREEN else RED
            val c2 = if (p2_on_the_right_way) GREEN else RED

            val v1 = (dp.p1.actualPos - dp.p2.actualPos).n
            val v2 = v1.perpendicular

            drawDashedLine(dp.p1.actualPos, dp.p1.actualPos + v2 * 100, 2.5, colorIfPlayerAliveOrRed(c1))
            drawDashedLine(dp.p2.actualPos, dp.p2.actualPos + v2 * 100, 2.5, colorIfPlayerAliveOrRed(c2))

            drawFilledCircle(dp.p1.actualPos, 0.3, colorIfPlayerAliveOrRed(RED))
            drawCircle(dp.p1.actualPos, dp.dock_dist, colorIfPlayerAliveOrRed(RED))
            drawFilledCircle(dp.p2.actualPos, 0.3, colorIfPlayerAliveOrRed(RED))
            drawCircle(dp.p2.actualPos, dp.dock_dist, colorIfPlayerAliveOrRed(RED))
          })
        )
        /*docking_points.foreach(dp => {
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
        })*/
      }
    }

    engines.foreach(e => drawEngine(e))
  }

  lazy val is_player: Boolean = index == player_ship.index

  private def drawShip(): Unit = {
    if (!drawMapMode && (is_player || coord.dist2(player_ship.coord) < 100L * 1000L * 100L * 1000L)) {
      if (isAlive) {
        openglLocalTransform {
          openglMove(coord - base)
          drawIfAliveBeforeRotation()
          openglRotateDeg(rotation)
          drawIfAliveAfterRotation()
        }
      } else {
        openglLocalTransform {
          openglMove(coord - base)
          openglRotateDeg(rotation)
          drawSlidingLines(actualDrawPoints, colorIfPlayerAliveOrRed(WHITE))
        }
      }
    }
  }

  private val render_id = render {
    if (currentState.active) {
      drawShip()
    }
  }

  def preserveAngularVelocity(ang_vel_deg: Double): Unit

  def preserveVelocity(vel: DVec): Unit

  /**
   * rotation и angle_deg оба в диапазоне 0 - 360
   * @param angle_deg - угол между ориентацией корабля и вектором DVec(0, 1), который необходимо поддерживать
   */
  def preserveAngle(angle_deg: Double): Unit = {
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
  protected var last_correction_or_check_moment: Long = 0L

  private var prev_flight_mode_and_engine_states: Option[(FlightMode, List[(Long, Double, Boolean)])] = None

  def haveSavedFlightMode: Boolean = prev_flight_mode_and_engine_states.nonEmpty

  private def saveFlightModeAndEngineStates(prev_flight_mode: FlightMode): Unit = {
    prev_flight_mode_and_engine_states = Some(
      (prev_flight_mode, engines.map(e => (e.workTimeTacts, e.power, e.active)))
    )
  }

  def restoreFlightModeAndEngineStates(): Unit = {
    prev_flight_mode_and_engine_states match {
      case Some((prev_flight_mode, engine_states)) =>
        flightMode = prev_flight_mode
        engines.zip(engine_states).foreach { case (e, (tacts, power, active)) =>
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

  def flightMode_=(new_flight_mode: FlightMode): Unit = {
    val prev_flight_mode = flight_mode
    flight_mode = new_flight_mode
    last_correction_or_check_moment = 0L
    if (flight_mode == Maneuvering) {
      val ten_min_or_max_time_at_full_power =
        math.min((fuelMass / engines.map(_.maxFuelConsumptionPerTact).max).toLong, 37800)
      if (interfaceHolder.dockingSwitcher.dockingEnabled) {
        engines.foreach(e => e.power = 10000)
      } else {
        engines.foreach(e =>
          e.power = {
            if (interfaceHolder.gSwitcher.maxGSet) {
              math.min(
                thisOrActualProxyShipMass * interfaceHolder.gSwitcher.maxG * earth.g + {
                  earth
                    .airResistance(
                      currentState,
                      earth.currentState, /*shipsHolder.currentShipStatesExceptShip(index), */ 28,
                      0.5
                    )
                    .norma
                },
                e.max_power * 0.5
              )
            } else {
              e.max_power * 0.5
            }
          }
        )
      }
      engines
        .filterNot(_.active)
        .foreach(e => e.workTimeTacts = ten_min_or_max_time_at_full_power) // 10 minutes in tacts (10*60*63)
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
      } else if (
        prev_flight_mode == Maneuvering &&
        (flight_mode == Killrot ||
          flight_mode == NearestShipVelocity ||
          flight_mode == NearestShipAligned ||
          flight_mode == RelativeVelocityAligned ||
          flight_mode == OppositeRelativeVelocityAligned)
      ) {
        saveFlightModeAndEngineStates(prev_flight_mode)
      } else if (flight_mode == NearestPlanetVelocity) {
        vertical_speed_msec = 0
        horizontal_speed_msec = 0
      }
      if (prev_flight_mode == Maneuvering && flight_mode == FreeFlightMode) {
        engines.foreach(e => e.workTimeTacts = 0)
      }
    }
  }

  def vertical_speed_msec: Int = 0

  def vertical_speed_msec_=(x: Int): Unit = {}

  def horizontal_speed_msec: Int = 0

  def horizontal_speed_msec_=(x: Int): Unit = {}

  def flightModeStr: String = flightMode match {
    case NearestPlanetVelocity =>
      s"уравнять скорость с ближайшей планетой: ${msecOrKmsecOrKmhour(vertical_speed_msec)}, ${msecOrKmsecOrKmhour(horizontal_speed_msec)}"
    case x => x.rusStr
  }

  /**
   * Все другие корабли, отсортированные по расстоянию по убыванию (первый - ближайший).
   * @return
   */
  protected def shipsNear: Seq[PolygonShip] = shipsHolder.ships
    .filter(s => {
      s.currentState.active &&
      s.thisOrActualProxyShipIndex != thisOrActualProxyShipIndex &&
      s.isAlive
    })
    .sortBy(s => coord.dist2(s.coord))

  /**
   * Корабли ближе x км от нас. Метод используется для вычисления автоматического наведения ракет.
   * @param x - расстояние в километрах
   * @return
   */
  protected def shipsCloserXKm(x: Long): Seq[PolygonShip] = shipsHolder.ships
    .filter(s => {
      s.currentState.active &&
      s.thisOrActualProxyShipIndex != thisOrActualProxyShipIndex &&
      s.isAlive &&
      s.coord.dist2(coord) < x * 1000L * x * 1000L
    })
    .sortBy(s => coord.dist2(s.coord))

  /**
   * Корабль ближе x км от нас. Если таких несколько, то ближайший
   * @param x - дистанция в километрах
   * @return
   */
  private def shipCloserXKm(x: Long): Option[PolygonShip] = shipsCloserXKm(11).headOption

  private def shipsCloser2Km: Seq[PolygonShip] = shipsCloserXKm(2)

  /**
   * Корабль ближе 1 км от нас. Если таких несколько, то ближайший.
   * Метод используется для реализации автоматической стыковки.
   * @return
   */
  private def shipCloser2Km: Option[PolygonShip] = shipCloserXKm(2)

  def tryDock: Boolean = false
  def tryUndock: Boolean = false

  private def possibleDockPointsWithNearestShip: List[(DockingPoints, PolygonShip, DockingPoints)] = {
    for {
      dp <- docking_points
      os <- shipsCloser2Km
      osdp <- os.docking_points
      if dp.pointsMatch(osdp)
    } yield (dp, os, osdp)
  }

  val pilot_mass = 75
  val pilot_position: DVec = DVec(0, 8)
  private var pilot_average_g: Double = 0.0
  private val pilot_accs = ArrayBuffer[(DVec, Long)]()

  def is_manned: Boolean

  private var is_dead = false
  private var death_reason = ""

  def deathReason: String = death_reason

  def isDead: Boolean = is_dead

  def isAlive: Boolean = !is_dead

  private var ship_is_crashed = false

  def isCrashed: Boolean = ship_is_crashed

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
    ans * TimeConstants.base_dt
  }

  def colorIfAliveOrRed(color: => ScageColor): ScageColor = if (isDead) RED else color

  def colorIfPlayerAliveOrRed(color: => ScageColor): ScageColor = if (Main.player_ship.isDead) RED else color

  private var main_ship_wreck: Option[Wreck] = None

  def kill(reason: String, crash: Boolean): Unit = {
    is_dead = true
    death_reason = reason
    flightMode = FreeFlightMode
    engines.foreach(_.active = false)
    /*if(this.index == Main.ship.index) {
      viewMode = FixedOnShipAbsolute
    }*/
    if (isDocked) {
      undock()
    }
    ship_interface.foreach(_.forceUpdate())
    if (crash) {
      shipsHolder.removeShip(this)
      delOperation(render_id)
      /*println(s"wreck_parts.map(_.area).sum == shape.area = ${wreck_parts.map(_.area).sum == shape.area}")
      println(s"wreck_parts.map(_.area).sum = ${wreck_parts.map(_.area).sum}")
      println(s"shape.area = ${shape.area}")*/
      val wrecks = wreck_parts.zipWithIndex.map { case (wreck_part, idx) =>
        val part_center = currentState.coord + wreck_part.points.sum / wreck_part.points.length
        val part_points = wreck_part.points.map(p => currentState.coord + p - part_center)
        val maybe_obstacle = currentState.contacts.headOption.map(c => if (c.a.index != index) c.a else c.b)
        val random_wreck_vel_func = wreckRandomVelocity(maybe_obstacle)
        // val wreck_mass = mass*wreck_part.area/shape.area
        val wreck_mass = mass / wreck_parts.length
        new Wreck(
          wreck_mass,
          part_center,
          random_wreck_vel_func(),
          rotation,
          part_points,
          is_main = idx == 0 && index == player_ship.index
        )
      }
      main_ship_wreck = wrecks.find(_.is_main)
      ship_is_crashed = true
    }
  }

  private def wreckRandomVelocity(maybe_obstacle: Option[MutableBodyState]): () => DVec = {
    maybe_obstacle match {
      case Some(obstacle) =>
        shipsHolder.shipByIndex(obstacle.index) match {
          case Some(ship_obstacle) =>
            val dir_deg = 140.0 + math.random * 80.0
            // val new_vel = ((mass - ship_obstacle.mass)*linearVelocity + 2*ship_obstacle.mass*ship_obstacle.linearVelocity)/(mass + ship_obstacle.mass)
            val new_vel =
              linearVelocity * mass / (mass + obstacle.mass) + obstacle.vel * obstacle.mass / (mass + obstacle.mass)
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
            val rate = 100.0 / 60 * TimeConstants.base_dt // восстановление после критической перегрузки за 60 секунд
            val time_to_restore_msec = (((100 - before_death_counter) / rate) * TimeConstants.base_dt * 1000).toLong
            f"Пилот в состоянии невесомости. Восстанавливается после перегрузки (${timeStrSec(time_to_restore_msec)})"
          } else {
            "Пилот в состоянии невесомости"
          }
        } else if (pilot_average_g <= 1.09) {
          if (before_death_counter != 100) {
            val rate = 100.0 / 60 * TimeConstants.base_dt // восстановление после критической перегрузки за 60 секунд
            val time_to_restore_msec = (((100 - before_death_counter) / rate) * TimeConstants.base_dt * 1000).toLong
            f"Пилот испытывает силу тяжести $pilot_average_g%.1fg. Восстанавливается после перегрузки (${timeStrSec(time_to_restore_msec)})"
          } else {
            f"Пилот испытывает силу тяжести $pilot_average_g%.1fg"
          }
        } else {
          if (pilot_average_g > 4) {
            val rate = deatchCounterChangeRate(pilot_average_g)
            val time_before_death_msec = ((before_death_counter / rate) * TimeConstants.base_dt * 1000).toLong
            f"[rПилот испытывает критическую перегрузку $pilot_average_g%.1fg. Смерть через ${timeStrSec(time_before_death_msec)}]"
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

  def massStr = s"Масса корабля: ${gOrKg(mass)}"
  def fuelMassStr = s"Остаток топлива: ${gOrKg(fuelMass)}"

  def shadowSideStr: String = {
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

  lazy val shape: PolygonShape = PolygonShape(points, convex_parts)

  lazy val draw_points: List[DVec] = points :+ points.head

  def actualDrawPoints: List[DVec] = {
    val (coord_diff, rotation_diff) = ourCoordAndRotationDiff
    draw_points.map(p => {
      if (rotation_diff != 0) {
        p.rotateDeg(rotation_diff) + coord_diff
      } else {
        p + coord_diff
      }
    })
  }

  /**
   * Точки, обозначающие корпус корабля, группируются по два, получаются отрезки. Для каждой точки вычисляется фактическая текущая координата.
   * Получается набор фактических текущих координат корпуса, собранных по два.
   * @return List<List<DVec>> - Во внутреннем списке всегда два элемента.
   */
  def curDrawLines: List[List[(DVec, Int)]] = draw_points.zipWithIndex
    .sliding(2)
    .map { case List((p1, p1idx), (p2, p2idx)) =>
      List((coord + p1.rotateDeg(rotation), p1idx), (coord + p2.rotateDeg(rotation), p2idx))
    }
    .toList

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
    shape = shape,
    is_static = false
  )

  lazy val currentState: MutableBodyState = initState.toMutableBodyState

  def thisOrActualProxyShipCurrentState: MutableBodyState =
    dock_data.map(_.proxy_ship.thisOrActualProxyShipCurrentState).getOrElse(currentState)

  private var ship_interface: Option[OtherShipInfo] = None
  def shipInterface: Option[OtherShipInfo] = ship_interface

  if (!ship_designer) {
    shipsHolder.addShip(this)
    if (create_interface) {
      ship_interface = Some(interfaceHolder.addShipInterface(this))
    }
  }

  private var _orbit_data: Option[OrbitRenderData] = None
  def orbitData: Option[OrbitRenderData] = _orbit_data

  def thisOrActualProxyShipOrbitData: Option[OrbitRenderData] =
    dock_data.map(_.proxy_ship.thisOrActualProxyShipOrbitData).getOrElse(_orbit_data)

  def updateOrbitData(
      update_count: Long,
      orbit_color: ScageColor,
      time_msec: Long,
      some_system_state: mutable.Map[Int, MutableBodyState],
      calculate_orbit_around: Option[Int] = None): Unit = {
    dock_data match {
      case Some(dd) =>
        dd.proxy_ship.updateOrbitData(update_count, orbit_color, time_msec, some_system_state, calculate_orbit_around)
      case None =>
        if (_orbit_data.isEmpty || _orbit_data.exists(_.update_count != update_count)) {
          if (currentState.active) {
            _orbit_data = OrbitDataUpdater.updateOrbitData(
              update_count,
              index,
              radius,
              orbit_color,
              some_system_state,
              planetIndices,
              calculate_orbit_around
            )
          } else {
            updateStateSinceDeactivation(time_msec, some_system_state)
            _orbit_data = OrbitDataUpdater.updateOrbitData(
              update_count,
              currentState,
              radius,
              orbit_color,
              some_system_state,
              planetIndices,
              calculate_orbit_around
            )
          }
          if (time_msec == system_evolution.timeMsec) {
            _current_orbit_data = _orbit_data
          } else {
            updateCurrentOrbitData(update_count, orbit_color, calculate_orbit_around)
          }
        }
    }
  }

  private def updateStateSinceDeactivation(
      time_msec: Long,
      some_system_state: mutable.Map[Int, MutableBodyState]): Unit = {
    _orbit_data match {
      case Some(or) =>
        val time_since_deactivation_msec = time_msec - deactivate_moment_msec
        val planet_coord = some_system_state.get(or.planet.index).map(_.coord).getOrElse(or.planet.coord)
        val planet_vel = some_system_state.get(or.planet.index).map(_.vel).getOrElse(or.planet.linearVelocity)
        val planet_ang_vel =
          some_system_state.get(or.planet.index).map(_.ang_vel).getOrElse(or.planet.currentState.ang_vel)
        if (!or.is_landed) {
          val new_orbit = or.orbit.withNewFocusPosition(planet_coord)
          /*val new_coord = new_e.orbitalPointAfterTime(deactivate_point_relative + planet_coord, time_since_deactivation_msec, or.ccw)
          if(new_coord.x.isNaN || new_coord.y.isNaN || new_coord == planet_coord) {
            val x = new_e.orbitalPointAfterTime(deactivate_point_relative + planet_coord, time_since_deactivation_msec, or.ccw)
          }*/
          currentState.coord = new_orbit.orbitalPointAfterTime(
            deactivate_point_relative + planet_coord,
            time_since_deactivation_msec,
            or.ccw
          )
          val v = new_orbit.orbitalVelocityInPoint(currentState.coord, or.ccw)
          currentState.vel = v + planet_vel
        } else {
          val ang_diff = planet_ang_vel / 1000 * time_msec
          currentState.coord = deactivate_point_relative.rotateDeg(ang_diff) + planet_coord
          currentState.vel = planet_vel + (currentState.coord - planet_coord).p * or.planet.groundSpeedMsec
        }
      case None =>
    }
  }

  private var _current_orbit_data: Option[OrbitRenderData] = None
  def currentOrbitData: Option[OrbitRenderData] = _current_orbit_data

  def thisOrActualProxyShipCurrentOrbitData: Option[OrbitRenderData] =
    dock_data.map(_.proxy_ship.thisOrActualProxyShipOrbitData).getOrElse(_current_orbit_data)

  private def updateCurrentOrbitData(
      update_count: Long,
      orbit_color: ScageColor,
      calculate_orbit_around: Option[Int]): Unit = {
    if (currentState.active) {
      _current_orbit_data = OrbitDataUpdater.updateOrbitData(
        update_count,
        index,
        radius,
        orbit_color,
        system_evolution.allBodyStates,
        planetIndices,
        calculate_orbit_around
      )
    } else {
      updateStateSinceDeactivation(system_evolution.timeMsec, system_evolution.allBodyStates)
      _current_orbit_data = OrbitDataUpdater.updateOrbitData(
        update_count,
        currentState,
        radius,
        orbit_color,
        system_evolution.allBodyStates,
        planetIndices,
        calculate_orbit_around
      )
    }
  }

  def beforeStep(): Unit = {
    if (currentState.active) {
      engines.foreach(e => {
        if (e.active) {
          if (e.workTimeTacts <= 0 || fuelMass <= 0) {
            e.active = false
          } else {
            if (fuelMass - e.fuelConsumptionPerTact <= 0) {
              e.active = false
            }
          }
        }
      })
      if (currentState.ang_vel != 0 && math.abs(currentState.ang_vel) < angular_velocity_error) {
        currentState.ang_vel = 0
      }
      currentState.mass = mass /*currentMass(_tacts)*/
    }
  }

  private var deactivate_moment_msec: Long = 0L
  private var deactivate_point_relative: DVec = DVec.zero

  def checkCriticalCollision(): Unit = {
    val dvel = currentState.dvel.norma
    if (dvel > 10) {
      // crash tolerance = 10 m/s
      val crash_g = dvel / TimeConstants.base_dt / earth.g
      kill(f"Корабль уничтожен в результате столкновения ($crash_g%.2fg)", crash = true)
    }
  }

  def updatePilotAverageG(reactive_force: DVec, time_msec: Long): Unit = {
    // ниже мы рассчитаем отдельно вертикальную и горизонтальную перегрузки и потом сложим их. Так надо считать, потому что к вертикальной перегрузке прибавляется центробежная сила, а к горизонтальной нет.
    val v_vert =
      pilot_position.actualPos.rotateDeg(thisOrActualProxyShipRotation).n // единичный вектор спина-грудь пилота
    val v_hor = -v_vert.perpendicular // единичный вектор левая рука - права рука пилота
    // центробежная сила от вращения корабля
    val centrifugial_force =
      if (thisOrActualProxyShipAngularVelocity == 0) 0.0
      else pilot_mass * math.pow(thisOrActualProxyShipAngularVelocity.toRad, 2) * pilot_position.actualPos.norma
    // reactive_force берем с минусом, потому что пилота вжимает под действием этой силы в противоположную сторону. Аналогично ускорение от коллизий
    val pilot_acc_vert =
      -reactive_force / mass * v_vert + centrifugial_force / pilot_mass - thisOrActualProxyShipCurrentState.dacc * v_vert
    val pilot_acc_hor = -reactive_force / mass * v_hor - thisOrActualProxyShipCurrentState.dacc * v_hor
    val pilot_acc =
      pilot_acc_vert * DVec(0, 1) + pilot_acc_hor * DVec(
        1,
        0
      ) // тут мы умножаем на единичные векторы в системе координат: начало в центре масс, вертикальный вектор - от центра масс к пилоту
    pilot_accs += ((pilot_acc, time_msec))
    if (time_msec - pilot_accs.head._2 >= 1000) {
      pilot_average_g = (pilot_accs.map(_._1).sum / pilot_accs.length).norma / earth.g
      pilot_accs.clear()
    }
  }

  private def checkPlanetCollision(): Unit = {
    // если провалились сквозь землю
    currentPlanetStates.find { case (planet, planet_state) =>
      planet.coord.dist2(currentState.coord) < planet.radius2
    } match {
      case Some((planet, planet_state)) =>
        currentState.coord =
          currentState.coord + (currentState.coord - planet.coord).n * (planet.radius + radius - planet.coord.dist(
            currentState.coord
          ))
        currentState.vel = planet.linearVelocity
        kill("Корабль врезался в планету", crash = true)
      case None =>
    }
  }

  private lazy val sun_critical_dist2 = math.pow(sun.radius + 30000000000.0, 2)

  private def checkSunDistance(): Unit = {
    // если подлетаем к поверхности Солнца ближе, чем 30 миллионов километров, то бууум!)
    if (coord.dist2(sun.coord) < sun_critical_dist2) {
      kill("Корабль слишком приблизился к Солнцу и сгорел", crash = true)
    }
  }

  def checkCriticalG(): Unit = {
    // пилот может испытывать перегрузку больше 4g только ограниченный период времени, потом наступает смерть
    // для беспилотной системы это значение примем 40g (условный показатель)
    if (
      pilot_average_g > {
        if (is_manned) 4 else 40
      }
    ) {
      // беспилотный корабль может выдерживать 10-кратно большие перегрузки по сравнению с пилотируемым
      val rate = deatchCounterChangeRate {
        if (is_manned) pilot_average_g else pilot_average_g / 10
      }
      before_death_counter -= rate
      if (before_death_counter <= 0) {
        if (is_manned) {
          kill(f"Пилот умер от сильной перегрузки ($pilot_average_g%.2fg)", crash = false)
        } else {
          kill(f"Корабль разрушился от сильной перегрузки ($pilot_average_g%.2fg)", crash = true)
        }
      } else {
        // автоматическое отключение двигателей, если до смерти от перегрузки осталась одна секунда
        val time_before_death_msec = ((before_death_counter / rate) * TimeConstants.base_dt * 1000).toLong
        if (time_before_death_msec <= 1000) {
          // 1 секунда до гибели от перегрузки
          if (engines.exists(_.active)) {
            // если работают какие-то двигатели
            flightMode = FreeFlightMode // отключаем двигатели
            engines.foreach(_.active = false)
          }
        }
      }
    } else {
      if (before_death_counter != 100) {
        if (pilot_average_g <= 1.09) {
          // пилот может восстанавливаться после перегрузки, только если текущая ускорение не выше 1.09g
          val rate = 100.0 / 60 * TimeConstants.base_dt // восстановление после критической перегрузки за 60 секунд
          before_death_counter += rate
          if (before_death_counter >= 100) {
            before_death_counter = 100
          }
        }
      }
    }
  }

  def checkEnginesPower(reactive_force: DVec): Unit = {
    // автоматическая регулировка мощности двигателей в соответствие с настройкой gSwitcher
    if (interfaceHolder.gSwitcher.maxGSet && pilot_average_g > interfaceHolder.gSwitcher.maxG) {
      val active_engines = engines.filter(e => e.active && 0 < e.stopMomentTacts)
      if (active_engines.nonEmpty) {
        val cur_force = reactive_force.norma
        val allowed_force = thisOrActualProxyShipMass * interfaceHolder.gSwitcher.maxG * earth.g
        if (cur_force > allowed_force) {
          val force_diff = cur_force - allowed_force
          val force_diff_for_engine = force_diff / active_engines.length
          active_engines.foreach(e =>
            if (force_diff_for_engine < e.power) {
              e.power -= force_diff_for_engine
            }
          )
        }
      }
    }
  }

  def consumeFuel(): Unit = {
    engines.foreach(e => {
      if (e.active) {
        if (e.workTimeTacts > 0) {
          e.workTimeTacts -= 1
          fuelMass -= e.fuelConsumptionPerTact
        } else e.active = false
      }
    })
  }

  def checkDockingSituation(): Unit = {
    if (tryDock) {
      dock()
    }
    if (tryUndock) {
      undock()
    }
  }

  /**
   * В этом методе мы:
   * проверяем, не произошло ли столкновение со скоростью больше 10 м/сек, если да, разваливаемся на части
   * вычисляем ускорение, которое испытывает пилот
   * проверяем, не провалились ли сквозь поверхность планеты, если да, уничтожаемся
   * проверяем, не подлетели ли к Солнцу ближе чем 30000000 км, если да, уничтожаемся
   * проверяем, не является ли текущая перегрузка критической, и не пора ли нам умирать от перегрузки
   * проверяем, не следует ли уменьшить тягу двигателей, чтобы уложиться по перегрузке в настройку gSwitcher
   * у всех работающих двигателей уменьшаем оставшееся время работы на один такт и потребляем топливо
   *
   * @param time_msec - текущее время симуляции, миллисекунды
   */
  private def calculateShipState(time_msec: Long): Unit = {
    // сила от реактивных двигателей и сила сопротивления воздуха
    val air_resistance = earth.airResistance(
      currentState,
      earth.currentState, /*shipsHolder.currentShipStatesExceptShip(index), */ 28,
      0.5
    )
    val reactive_force = currentReactiveForce(0, currentState) + air_resistance
    if (!ship_is_crashed) {
      checkCriticalCollision()
      if (is_dead) return
      updatePilotAverageG(reactive_force, time_msec)
      checkPlanetCollision()
      if (is_dead) return
      checkSunDistance()
      if (is_dead) return
    }
    if (!is_dead) {
      checkCriticalG()
      if (is_dead) return
      checkEnginesPower(reactive_force)
    }
    consumeFuel()
    checkDockingSituation()
  }

  def syncOtherEnginesPower(except_engine_index: Int): Unit = {
    println(s"syncOtherEnginesPower(except_engine=$except_engine_index)")
    if (interfaceHolder.gSwitcher.maxGSet) {
      val active_engines_except =
        engines.filter(e => e.active && 0 < e.stopMomentTacts && e.index != except_engine_index)
      if (active_engines_except.nonEmpty) {
        val air_resistance = earth.airResistance(
          currentState,
          earth.currentState, /*shipsHolder.currentShipStatesExceptShip(index), */ 28,
          0.5
        )
        val reactive_force = currentReactiveForce(0, currentState) + air_resistance
        val cur_force = reactive_force.norma
        val allowed_force = thisOrActualProxyShipMass * interfaceHolder.gSwitcher.maxG * earth.g
        if (cur_force > allowed_force) {
          val force_diff = cur_force - allowed_force
          val force_diff_for_engine = force_diff / active_engines_except.length
          active_engines_except.foreach(e =>
            if (force_diff_for_engine < e.power) {
              e.power -= force_diff_for_engine
            }
          )
        }
      }
    }
  }

  private val conditions: List[() => Boolean] = List(
    () => thisOrActualProxyShipIndex != player_ship.thisOrActualProxyShipIndex,
    () => coord.dist2(Main.player_ship.coord) > 500L * 1000L * 500L * 1000L,
    () =>
      _orbit_data.exists(or => {
        or.is_landed || or.orbit.r_p > or.planet.radius + or.planet.air_free_altitude
      }),
    () => engines.forall(!_.active)
  )

  def afterStep(time_msec: Long): Unit = {
    if (dock_data.isEmpty) {
      // условие сделать корабль неактивным и не обрабатывать его:
      // если это не корабль игрока, расстояние от данного корабля до корабля игрока больше 500 км,
      // перигей орбиты выше верхней границы атмосферы (орбита стабильная) или мы стоим на земле,
      // двигатели не включены
      val deactivate_condition = thisOrActualProxyShipIndex != player_ship.thisOrActualProxyShipIndex &&
        coord.dist2(Main.player_ship.coord) > 500L * 1000L * 500L * 1000L &&
        _orbit_data.exists(or => {
          or.is_landed || or.orbit.r_p > or.planet.radius + or.planet.air_free_altitude
        }) &&
        engines.forall(!_.active)
      // val (deactivate_condition, reason) = deactivateCondition
      if (currentState.active) {
        calculateShipState(time_msec)
        if (deactivate_condition) {
          currentState.active = false
          deactivate_moment_msec = time_msec
          deactivate_point_relative = coord - _orbit_data.map(_.planet.coord).getOrElse(DVec.zero)
          println(s"deactivated $name")
        }
      } else {
        if (!deactivate_condition || (drawMapMode && ship_interface.exists(!_.isMinimized))) {
          updateStateSinceDeactivation(time_msec, system_evolution.allBodyStates)
        }
        if (!deactivate_condition) {
          currentState.active = true
          println(s"activated $name")
          // println(s"activated $name, reason: $reason")
        }
      }
    } else {
      dock_data.foreach(_.proxy_ship.updateShipState(index))
    }
  }
}
