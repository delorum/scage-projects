package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

import scala.collection.mutable.ArrayBuffer

class Ship4(index:Int,
            init_coord:DVec,
            init_velocity:DVec = DVec.dzero,
            init_rotation:Double = 0.0) extends PolygonShip(index, "Снежинка", init_coord, init_velocity, init_rotation) {
  private val _payload:Double = 5*1000
  private var _fuel_mass:Double = 5*1000
  def mass:Double = _payload + _fuel_mass
  override def fuelMass: Double = _fuel_mass
  override def fuelMass_=(m: Double): Unit = {_fuel_mass = m}

  val points:List[DVec] = List(
    DVec(3.5, 2.5),
    DVec(1.5, 6.5),
    DVec(1.5, 10.5),
    DVec(-1.5, 10.5),
    DVec(-1.5, 6.5),
    DVec(-3.5, 2.5),
    DVec(-3.5, -3.5),
    DVec(3.5, -3.5)
  )

  val convex_parts = List(
    PolygonShape(List(DVec(-3.5, -3.5), DVec(3.5, -3.5), DVec(3.5, 2.5), DVec(-3.5, 2.5)), Nil),
    PolygonShape(List(DVec(-3.5, 2.5), DVec(3.5, 2.5), DVec(1.5, 6.5), DVec(-1.5, 6.5)), Nil),
    PolygonShape(List(DVec(-1.5, 10.5), DVec(-1.5, 6.5), DVec(1.5, 6.5), DVec(1.5, 10.5)), Nil)
  )

  val wreck_parts = List(
    PolygonShape(List(DVec(-3.5, -3.5), DVec(-0.5, -3.5), DVec(-0.5, -0.5), DVec(-3.5, -0.5)), Nil),
    PolygonShape(List(DVec(-0.5, -3.5), DVec(3.5, -3.5), DVec(3.5, -0.5), DVec(-0.5, -0.5)), Nil),
    PolygonShape(List(DVec(-3.5, 2.5), DVec(-3.5, -0.5), DVec(0.5, -0.5), DVec(0.5, 2.5)), Nil),
    PolygonShape(List(DVec(0.5, -0.5), DVec(3.5, -0.5), DVec(3.5, 2.5), DVec(0.5, 2.5)), Nil),
    PolygonShape(List(DVec(-3.5, 2.5), DVec(-0.5, 2.5), DVec(-0.5, 6.5), DVec(-1.5, 6.5)), Nil),
    PolygonShape(List(DVec(-0.5, 2.5), DVec(3.5, 2.5), DVec(1.5, 6.5), DVec(-0.5, 6.5)), Nil),
    PolygonShape(List(DVec(-1.5, 6.5), DVec(1.5, 6.5), DVec(1.5, 8.5), DVec(-1.5, 8.5)), Nil),
    PolygonShape(List(DVec(-1.5, 8.5), DVec(1.5, 8.5), DVec(1.5, 10.5), DVec(-1.5, 10.5)), Nil)
  )

  val docking_points = List(new DockingPoints(DVec(-1.5, 10.5), DVec(1.5, 10.5), this))

  // миллион ньютонов тяги при расходе 4 килограмма в секунду - это соответствует скорости истечения газов 250 км/сек
  // что в 50 раз выше наивысшего полученного на практике значения для химического топлива: литий/водород/фтор - 5000 м/сек
  val four  = new Engine("4",  Vec(-3.5, 0.0), Vec(1.0, 0.0),  1000000, 1,   4,    this)
  val six   = new Engine("6",  Vec(3.5, 0.0),  Vec(-1.0, 0.0), 1000000, 1,   4,    this)
  val seven = new Engine("7",  Vec(-1.5, 9.0), Vec(1.0, 0.0),  10000,   100, 0.04, this)
  val nine  = new Engine("9",  Vec(1.5, 9.0),  Vec(-1.0, 0.0), 10000,   100, 0.04, this)
  val eight = new Engine("8",  Vec(0.0, 10.5), Vec(0.0, -1.0), 1000000, 1,   4,    this)
  val two   = new Engine("2",  Vec(0.0, -3.5), Vec(0.0, 1.0),  1000000, 1,   4,    this)

  val engines = List(four, six, seven, nine, eight, two)

  val engines_mapping = Map(
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two
  )

  private val linear_velocity_error = 0.1
  private val angular_velocity_error = 0.01
  private val angle_error = 0.1

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
  private def howManyTacts(to:Double, from:Double, a:Double, dt:Double):(Int, Double) = {
    val tacts = ((to - from)/(a*dt)).toInt + 1
    val result_to = from + tacts*a*dt
    //println(s"$from -> $to : $result_to : $tacts")
    (tacts, result_to)
    /*if(a == 0) tacts
    else if(a > 0) {
      if(from >= to) tacts
      else howManyTacts(to, from + a*dt, a, base_dt, tacts+1)
    } else {
      if(from <= to) tacts
      else howManyTacts(to, from + a*dt, a, base_dt, tacts+1)
    }*/
  }

  /**
   * Рассчитывает значение тяги двигателя, которую можно развить, чтобы за минимально возможное количество тактов работы этого двигателя
   * достичь требуемой скорости
   *
   * @param max_power - максимальная сила, которую можем приложить (максимальная тяга двигателя)
   * @param force_dir - фактически, 1 или -1
   * @param mass - наша масса
   * @param to - скорость, которой хотим достичь
   * @param from - наша текущая скорость
   * @param max_diff - максимальная допустимая разница между скоростью, которой достигнем, и желаемой скоростью
   * @return
   */
  private def maxPossiblePowerForLinearMovement(max_power:Double, force_dir:Double, mass:Double, to:Double, from:Double, max_diff:Double):Double = {
    val max_percent = { // сколько процентов тяги двигателя максимально можем использовать в соответствии с ограничением InterfaceHolder.gSwitcher
      if(InterfaceHolder.gSwitcher.maxG == -1) 99
      else {
        math.min(mass * InterfaceHolder.gSwitcher.maxG * OrbitalKiller.earth.g / max_power * 99, 99).toInt
      }
    }

    (max_percent to 1 by -1).find {
      case percent =>
        val power = max_power*0.01*percent
        val force = force_dir*power
        val acc = force / mass
        val (_, result_to) = howManyTacts(to, from, acc, base_dt)
        math.abs(to - result_to) < max_diff
    }.map(percent => max_power*0.01*percent).getOrElse(max_power*0.01)
  }

  private def maxPossiblePowerForRotation(max_power:Double, force_dir:DVec, position:DVec, I:Double, to:Double, from:Double, max_diff:Double):Double = {
    (99 to 1 by -1).find {
      case percent =>
        val power = max_power*0.01*percent
        val torque = (-force_dir*power)*/position
        val ang_acc = (torque / I).toDeg
        val (_, result_to) = howManyTacts(to, from, ang_acc, base_dt)
        math.abs(to - result_to) < max_diff
    }.map(percent => max_power*0.01*percent).getOrElse(max_power*0.1)
  }

  override def preserveAngularVelocity(ang_vel_deg: Double) {
    val difference = angularVelocity - ang_vel_deg
    if(difference > angular_velocity_error) {
      val power = maxPossiblePowerForRotation(seven.max_power, seven.force_dir, seven.position, currentState.I, ang_vel_deg, angularVelocity, angular_velocity_error)
      seven.power = power
      //six.power = power
      val ang_acc = (seven.torque / currentState.I).toDeg
      val (tacts, _) = howManyTacts(ang_vel_deg, angularVelocity, ang_acc, base_dt)
      activateOnlyTheseEngines(seven/*, six*/)
      seven.workTimeTacts = tacts
      //six.workTimeTacts = tacts
    } else if(difference < -angular_velocity_error) {
      val power = maxPossiblePowerForRotation(nine.max_power, nine.force_dir, nine.position, currentState.I, ang_vel_deg, angularVelocity, angular_velocity_error)
      nine.power = power
      //four.power = power
      val ang_acc = (nine.torque / currentState.I).toDeg
      val (tacts, _) = howManyTacts(ang_vel_deg, angularVelocity, ang_acc, base_dt)
      activateOnlyTheseEngines(nine/*, four*/)
      nine.workTimeTacts = tacts
      //four.workTimeTacts = tacts
    }
    last_correction_or_check_moment = OrbitalKiller.tacts
  }

  override def preserveVelocity(vel: DVec) {
    val n = DVec(0, 1).rotateDeg(rotation).n
    val p = n.p*(-1)

    val ship_velocity_n = linearVelocity*n  // from
    val ss_n = vel*n                         // to

    val activate_engines = ArrayBuffer[Engine]()

    if(ship_velocity_n - ss_n > linear_velocity_error) {
      val power = maxPossiblePowerForLinearMovement(eight.max_power, eight.force_dir.y, mass, ss_n, ship_velocity_n, linear_velocity_error)

      eight.power = power
      val acc = (eight.force / mass).y
      val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, base_dt)
      /*println("===========================")
      println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
      eight.workTimeTacts = tacts
      activate_engines += eight
    } else if(ship_velocity_n - ss_n < -linear_velocity_error) {
      val power = maxPossiblePowerForLinearMovement(two.max_power, two.force_dir.y, mass, ss_n, ship_velocity_n, linear_velocity_error)
      two.power = power
      val acc = (two.force / mass).y
      val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, base_dt)
      /*println("===========================")
      println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
      two.workTimeTacts = tacts
      activate_engines += two
    }

    val ship_velocity_p = p*linearVelocity
    val ss_p = p*vel

    if(ship_velocity_p - ss_p > linear_velocity_error) {
      val power = maxPossiblePowerForLinearMovement(six.max_power, six.force_dir.x, mass, ss_p, ship_velocity_p, linear_velocity_error)
      six.power = power
      val acc = (six.force / mass).x
      val (tacts, _) = howManyTacts(ss_p, ship_velocity_p, acc, base_dt)
      /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
      println("===========================")*/
      six.workTimeTacts = tacts
      activate_engines += six
    } else if(ship_velocity_p - ss_p < -linear_velocity_error) {
      val power = maxPossiblePowerForLinearMovement(four.max_power, four.force_dir.x, mass, ss_p, ship_velocity_p, linear_velocity_error)
      four.power = power
      val acc = (four.force / mass).x
      val (tacts, _) = howManyTacts(ss_p, ship_velocity_p, acc, base_dt)
      /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
      println("===========================")*/
      four.workTimeTacts = tacts
      activate_engines += four
    }
    activateOnlyTheseEngines(activate_engines:_*)
    last_correction_or_check_moment = OrbitalKiller.tacts
  }

  override var vertical_speed_msec:Int = 0
  override var horizontal_speed_msec:Int = 0

  private def angleMinDiff(angle1:Double, angle2:Double):Double = {
    val x = math.abs(angle1 - angle2)
    if(x > 180) 360 - x else x
  }

  val x = ArrayBuffer[DVec]()

  override def updateShipState(time_msec:Long): Unit = {
    super.updateShipState(time_msec)
    if(InterfaceHolder.dockingSwitcher.dockingEnabled) {
      if(canDockWithNearestShip && notDocked &&
        (InterfaceHolder.dockingSwitcher.dockingAuto || (InterfaceHolder.dockingSwitcher.dockingManual && InterfaceHolder.dockUndock.needDock))) {
        dock()
        if(isDocked) {
          InterfaceHolder.dockUndock.setDocked()
          InterfaceHolder.dockingSwitcher.setDockingManual()
        }
      } else if(isDocked && InterfaceHolder.dockUndock.needUndock) {
        undock()
      }
    }
    if(time_msec % 10 == 0) {
      val prev = x.lastOption
      val p = coord + DVec(0, 8).rotateDeg(rotation)
      x += p
      prev.foreach(pp => println(s"${mOrKmOrMKm(p.dist(pp))}"))
    }
  }

  render {
    openglLocalTransform {
      openglMove(coord - base)
      drawSlidingLines(x.map(_ - coord), WHITE)
    }
  }

  override def initState:BodyState = BodyState(
    index,
    mass,
    acc = DVec.zero,
    vel = init_velocity,
    coord = init_coord,
    ang_acc = 0,
    ang_vel = math.sqrt(5.0*earth.g/8.0).toDeg,
    ang = init_rotation,
    shape = PolygonShape(points, convex_parts),
    is_static = false)

  private def decideSpeedValue(dist:Double):Double = {
    val dist_abs = math.abs(dist)
    // разные значения скоростей для разных ограничений перегрузок - потому что если ограничение на перегрузку, можем просто не успеть затормозить с высокой скорости и врезаться
    val max_speed1 = InterfaceHolder.gSwitcher.maxG match {
      case 1 => 25
      case 4 => 50
      case _ => 100
    }
    val max_speed2 = InterfaceHolder.gSwitcher.maxG match {
      case 1 => 15
      case _ => 50
    }
    val max_speed3 = InterfaceHolder.gSwitcher.maxG match {
      case 1 => 10
      case _ => 25
    }
    val ans = if(dist_abs > 500) max_speed1
    else if(dist_abs > 250) max_speed1
    else if(dist_abs > 125) max_speed2
    else if(dist_abs > 50) max_speed3
    else if(dist_abs > 25) 10
    else if(dist_abs > 10) 5
    /*else if(dist_abs > 5) 2*/
    else 2
    if(dist >= 0) ans else -ans
  }

  action {
    if(fuelMass <= 0 && flightMode != FreeFlightMode) {
      flightMode = FreeFlightMode
    } else {
      flightMode match {
        case FreeFlightMode => // свободный режим
        case Killrot => // запрет вращения
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
              else flightMode = FreeFlightMode
            } else {
              preserveAngularVelocity(0)
            }
          }
        case VelocityAligned => // ориентация по траектории
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            val angle = DVec(0, 1).deg360(relativeLinearVelocity)
            if (angleMinDiff(rotation, angle) < angle_error) {
              if (math.abs(angularVelocity) < angular_velocity_error) {
                if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
              } else {
                preserveAngularVelocity(0)
              }
            }
            else preserveAngle(angle)
          }
        case OppositeVelocityAligned => // ориентация против траектории
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            val angle = DVec(0, -1).deg360(relativeLinearVelocity)
            if (angleMinDiff(rotation, angle) < angle_error) {
              if (math.abs(angularVelocity) < angular_velocity_error) {
                if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
              } else {
                preserveAngularVelocity(0)
              }
            }
            else preserveAngle(angle)
          }
        case CirclularOrbit => // выход на орбиту
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
                case Some((planet, planet_state)) =>
                  val ss = satelliteSpeed(coord, linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G)
                  if (linearVelocity.dist(ss) > linear_velocity_error) {
                    preserveVelocity(ss)
                  } else flightMode = FreeFlightMode
                case None =>
                  flightMode = FreeFlightMode
              }
            } else preserveAngularVelocity(0)
          }
        case NearestShipVelocity => // уравнять скорость с ближайшим кораблем
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              otherShipsNear.headOption match {
                case Some(s) =>
                  val ss = s.linearVelocity
                  if (linearVelocity.dist(ss) > linear_velocity_error) {
                    preserveVelocity(ss)
                  } else {
                    if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
                    else flightMode = FreeFlightMode
                  }
                case None =>
                  if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
                  else flightMode = FreeFlightMode
              }
            } else preserveAngularVelocity(0)
          }
        case NearestShipAligned => // ориентация на ближайший корабль
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            otherShipsNear.headOption match {
              case Some(os) =>
                val angle = DVec(0, 1).deg360(os.coord - coord)
                if (angleMinDiff(rotation, angle) < angle_error) {
                  if (math.abs(angularVelocity) < angular_velocity_error) {
                    if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
                    else flightMode = FreeFlightMode
                  } else {
                    preserveAngularVelocity(0)
                  }
                } else preserveAngle(angle)
              case None =>
                if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
            }
          }
        case NearestShipAutoDocking =>
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            // 0. если пристыкованы - ничего не делаем
            // 1. определяем ближайшую точку стыковки
            // 2. определяем ориентацию относительно стыковочного узла. Если не соориентированы, ориентируемся параллельно ей
            // 3. смотрим, где мы находимся по вертикали линии стыковки: за центром масс, между центром масс и точкой стыковки, ниже точки стыковки.
            // Если ниже точки стыковки, то все ок, иначе даем импульс, чтобы переместиться ниже.
            // 4. Смотрим, где мы находимся на перпендикуляре линии стыковки. Если требуется, даем импульс и движемся вправо или влево.
            // 5. Если мы на линии стыковки ниже точки стыковки, то даем импульс и движемся наверх.
            // 6. Если мы в зоне стыковки - стыкуемся, и переводим режим полета в "свободный".
            if(isDocked) {
              flightMode = FreeFlightMode
            } else {
              InterfaceHolder.dockingSwitcher.setDockingAuto()
              otherShipsNear.headOption match {
                case Some(os) =>
                  val ship_docking_point = docking_points.head.curP1 + 0.5*(docking_points.head.curP2 - docking_points.head.curP1)
                  os.docking_points.sortBy(osdp => osdp.curP1.dist(ship_docking_point)).headOption match {
                    case Some(osdp) =>
                      if(osdp.curP1.dist(ship_docking_point) > 2000) {  // система стыковки начинает работать с расстояния двух километров
                        flightMode = FreeFlightMode
                      } else {
                        val vv1 = (osdp.curP1-osdp.curP2).n
                        val docking_point = osdp.curP1 + 0.5*(osdp.curP2 - osdp.curP1)
                        val docking_dir = -vv1.perpendicular
                        val angle = DVec(0, 1).deg360(docking_dir)
                        if (angleMinDiff(rotation, angle) < angle_error) {
                          if (math.abs(angularVelocity) < angular_velocity_error) {
                            val A = ship_docking_point.x
                            val B = ship_docking_point.y
                            val C = docking_point.x
                            val D = docking_point.y
                            val a1 = vv1.x
                            val a2 = vv1.y
                            val b1 = docking_dir.x
                            val b2 = docking_dir.y
                            // координаты точки стыковки корабля в системе координат с началом в docking_point и базисными векторами (vv1, docking_dir)
                            val x = (b2*(A-C) - b1*(B-D))/(a1*b2 - a2*b1)
                            val y = (a2*(A-C) - a1*(B-D))/(a2*b1 - a1*b2)
                            if(y > 0) { // если мы выше точки стыковки
                              // летим вниз пока не окажемся на 20 метров ниже линии стыковки
                              preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y - (-20)))
                            } else {
                              if(math.abs(x) <= 0.2) {  // если мы ниже точки стыковки и на линии стыковки
                                // летим стыковаться
                                preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y))
                              } else if(math.abs(x) <= 1) { // если мы ниже точки стыковки и не очень далеко в стороне от линии стыковки
                                // движемся в сторону линии стыковки и в сторону точки стыковки (продолжаем стыковаться)
                                preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y) - vv1 * decideSpeedValue(x))
                              } else { // если мы ниже точки стыковки и далеко от линии стыковки
                                // летим пока не окажемся на 20 метров ниже линии стыковки и одновременно движемся в сторону линии стыковки
                                preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y - (-20)) - vv1 * decideSpeedValue(x))
                              }
                            }
                          } else {
                            preserveAngularVelocity(0)
                          }
                        } else {
                          preserveAngle(angle)
                        }
                      }
                    case None =>
                      flightMode = FreeFlightMode
                  }
                case None =>
                  flightMode = FreeFlightMode
              }
            }
          }
        case NearestPlanetVelocity => // уравнять скорость с ближайшей планетой
          (for {
            (planet, planet_state) <- currentPlanetStates.sortBy(_._2.coord.dist(coord)).headOption
          } yield (planet, planet_state)) match {
            case Some((planet, planet_state)) =>
              val vertical_orientation = DVec(0, 1).deg360(coord - planet_state.coord)
              if (angleMinDiff(rotation, vertical_orientation) >= angle_error) {
                preserveAngle(vertical_orientation)
              } else if (math.abs(angularVelocity) >= angular_velocity_error) {
                preserveAngularVelocity(0)
              } else {
                val ship_vertical_speed = (linearVelocity - planet_state.vel) * (coord - planet_state.coord).n
                val ship_above_ground_velocity = (linearVelocity - planet_state.vel) * (coord - planet_state.coord).p
                val vertical_diff = math.abs(ship_vertical_speed - vertical_speed_msec)
                val horizontal_diff = math.abs(ship_above_ground_velocity - horizontal_speed_msec - planet.groundSpeedMsec)
                if (vertical_diff > linear_velocity_error) {
                  if (horizontal_diff > linear_velocity_error) {
                    if (vertical_diff > horizontal_diff) {
                      preserveVelocity(
                        (planet_state.vel * (coord - planet_state.coord).n + vertical_speed_msec) * (coord - planet_state.coord).n +
                        (linearVelocity * (coord - planet_state.coord).p) * (coord - planet_state.coord).p
                      )
                    } else {
                      preserveVelocity((planet.groundSpeedMsec + horizontal_speed_msec) * (coord - planet_state.coord).p + planet_state.vel)
                    }
                  } else {
                    preserveVelocity(
                      (planet_state.vel * (coord - planet_state.coord).n + vertical_speed_msec) * (coord - planet_state.coord).n +
                      (linearVelocity * (coord - planet_state.coord).p) * (coord - planet_state.coord).p
                    )
                  }
                } else {
                  if (horizontal_diff > linear_velocity_error) {
                    preserveVelocity((planet.groundSpeedMsec + horizontal_speed_msec) * (coord - planet_state.coord).p + planet_state.vel)
                  } else {}
                }
              }
            case None =>
          }
        case _ =>
      }
    }
  }

  override protected def drawShip(): Unit = {
    if(!drawMapMode) {
      if(pilotIsAlive) {
        openglLocalTransform {
          openglMove(coord - base)
          drawFilledCircle(DVec.zero, 0.3, colorIfPlayerAliveOrRed(GREEN)) // mass center

          if (OrbitalKiller.globalScale >= 0.8) {
            if (!InterfaceHolder.linearVelocityInfo.isMinimized) {

              // current velocity
              drawArrow(DVec.zero, linearVelocity.n * 20, colorIfPlayerAliveOrRed(BLUE))
              drawArrow(DVec.zero, relativeLinearVelocity.n * 20, colorIfPlayerAliveOrRed(InterfaceHolder.linearVelocityInfo.color))
            }
            //drawArrow(DVec.zero, linearAcceleration.n * 100, ORANGE)        // current acceleration
            if (!InterfaceHolder.sunRelativeInfo.isMinimized) {
              // direction to earth
              drawArrow(Vec.zero, (sun.coord - coord).n * 20, colorIfPlayerAliveOrRed(InterfaceHolder.sunRelativeInfo.color))
            }
            if (!InterfaceHolder.earthRelativeInfo.isMinimized) {
              // direction to earth
              drawArrow(Vec.zero, (earth.coord - coord).n * 20, colorIfPlayerAliveOrRed(InterfaceHolder.earthRelativeInfo.color))
            }
            if (!InterfaceHolder.moonRelativeInfo.isMinimized) {
              // direction to moon
              drawArrow(Vec.zero, (moon.coord - coord).n * 20, colorIfPlayerAliveOrRed(InterfaceHolder.moonRelativeInfo.color))
            }
            InterfaceHolder.ship_interfaces.foreach(si => {
              if(!si.isMinimized) {
                drawArrow(Vec.zero, (si.monitoring_ship.coord - coord).n * 20, colorIfPlayerAliveOrRed(si.color))
              }
            })
          }

          /*val pa = (earth.coord - coord).n*(coord.dist(earth.coord) - earth.radius) + (earth.coord - coord).p*70000
          val pb = (earth.coord - coord).n*(coord.dist(earth.coord) - earth.radius) + (earth.coord - coord).p*(-70000)
          drawLine(pa, pb, WHITE)*/

          openglRotateDeg(rotation)

          // ниже алгоритм рисует линии корпуса корабля темносерым или белым в зависимости, в тени эта линия или нет
          /*val cur_draw_lines =  curDrawLines
          val cur_sun_coord = sun.coord
          draw_points.zipWithIndex.sliding(2).foreach {
            case List((p1, p1idx), (p2, p2idx)) =>
              val curP1 = coord + p1.rotateDeg(rotation)
              val curP1InShadow = inShadowOfPlanet(curP1).nonEmpty || cur_draw_lines.filterNot(x => x(0)._1 == curP1 || x(1)._1 == curP1).exists(x => {
                val res = areLinesIntersect(curP1, cur_sun_coord, x(0)._1, x(1)._1)
                res
              })
              val curP2 = coord + p2.rotateDeg(rotation)
              val curP2InShadow = inShadowOfPlanet(curP1).nonEmpty || cur_draw_lines.filterNot(x => x(0)._1 == curP2 || x(1)._1 == curP2).exists(x => {
                areLinesIntersect(curP2, cur_sun_coord, x(0)._1, x(1)._1)
              })
              if(!curP1InShadow && !curP2InShadow) {
                drawLine(p1, p2, colorIfAliveOrRed(WHITE))
              } else {
                drawLine(p1, p2, colorIfAliveOrRed(DARK_GRAY))
              }
              /*print(s"$p1idx", p1.toVec, color = WHITE, size = (max_font_size / globalScale).toFloat)
              print(s"$p2idx", p2.toVec, color = WHITE, size = (max_font_size / globalScale).toFloat)*/
          }*/

          drawSlidingLines(draw_points, colorIfPlayerAliveOrRed(WHITE))

          if (OrbitalKiller.globalScale >= 0.8) {
            if(isDocked) {
              dockData.foreach(d => {
                drawFilledCircle(d.our_dp.p1, 0.3, colorIfPlayerAliveOrRed(GREEN))
                drawFilledCircle(d.our_dp.p2, 0.3, colorIfPlayerAliveOrRed(GREEN))
              })
            } else if(InterfaceHolder.dockingSwitcher.dockingEnabled) {
              docking_points.foreach(dp => {
                drawFilledCircle(dp.p1, 0.3, colorIfPlayerAliveOrRed(RED))
                drawFilledCircle(dp.p2, 0.3, colorIfPlayerAliveOrRed(RED))
              })
            }
          }

          engines.foreach {
            case e => drawEngine(e)
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
              drawSlidingLines(mbs_points :+ mbs_points.head, colorIfPlayerAliveOrRed(RED))
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
}
