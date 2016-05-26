package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

import scala.collection.mutable.ArrayBuffer

class Ship4(index: Int,
            init_coord: DVec,
            init_velocity: DVec = DVec.dzero,
            init_rotation: Double = 0.0,
            ship_designer:Boolean = false) extends PolygonShip(index, "Снежинка", init_coord, init_velocity, init_rotation, ship_designer, false) {
  private var _payload: Double = 5 * 1000
  private var _fuel_mass: Double = 1000

  def mass: Double = _payload + _fuel_mass

  override def fuelMass: Double = _fuel_mass

  override def fuelMass_=(m: Double): Unit = {
    _fuel_mass = m
  }

  val is_manned = true

  lazy val engine_size: Double = 0.7

  lazy val points: List[DVec] = List(
    DVec(4.5, -4.5),
    DVec(4.5, -2.5),
    DVec(3.5, -1.5),
    DVec(3.5, 1.5),
    DVec(1.5, 5.5),
    DVec(1.5, 9.5),
    DVec(-1.5, 9.5),
    DVec(-1.5, 5.5),
    DVec(-3.5, 1.5),
    DVec(-3.5, -1.5),
    DVec(-4.5, -2.5),
    DVec(-4.5, -4.5),
    DVec(-3.5, -4.5),
    DVec(-2.5, -5.5),
    DVec(2.5, -5.5),
    DVec(3.5, -4.5)
  )

  lazy val convex_parts = List(
    PolygonShape(List(DVec(-4.5, -4.5), DVec(-3.5, -4.5), DVec(-3.5, -1.5), DVec(-4.5, -2.5)), List()),
    PolygonShape(List(DVec(-3.5, -4.5), DVec(-2.5, -5.5), DVec(2.5, -5.5), DVec(3.5, -4.5), DVec(3.5, 1.5), DVec(1.5, 5.5), DVec(-1.5, 5.5), DVec(-3.5, 1.5)), List()),
    PolygonShape(List(DVec(3.5, -4.5), DVec(4.5, -4.5), DVec(4.5, -2.5), DVec(3.5, -1.5)), List()),
    PolygonShape(List(DVec(-1.5, 5.5), DVec(1.5, 5.5), DVec(1.5, 9.5), DVec(-1.5, 9.5)), List())
  )

  val wreck_parts = List(
    PolygonShape(List(DVec(-4.5, -4.5), DVec(-1.5, -4.5), DVec(-4.5, -2.5)), List()),
    PolygonShape(List(DVec(-4.5, -2.5), DVec(-1.5, -4.5), DVec(0.5, -3.5), DVec(0.5, -0.5)), List()),
    PolygonShape(List(DVec(-4.5, -2.5), DVec(0.5, -0.5), DVec(-0.5, 2.5), DVec(-3.5, -1.5)), List()),
    PolygonShape(List(DVec(-3.5, -1.5), DVec(-0.5, 2.5), DVec(-3.5, 1.5)), List()),
    PolygonShape(List(DVec(-3.5, 1.5), DVec(-0.5, 2.5), DVec(0.5, 6.5), DVec(-1.5, 5.5)), List()),
    PolygonShape(List(DVec(-1.5, 5.5), DVec(0.5, 6.5), DVec(-0.5, 8.5), DVec(-1.5, 7.5)), List()),
    PolygonShape(List(DVec(-1.5, 7.5), DVec(-0.5, 8.5), DVec(0.5, 9.5), DVec(-1.5, 9.5)), List()),
    PolygonShape(List(DVec(-0.5, 8.5), DVec(0.5, 6.5), DVec(1.5, 7.5), DVec(1.5, 9.5), DVec(0.5, 9.5)), List()),
    PolygonShape(List(DVec(-0.5, 2.5), DVec(1.5, 5.5), DVec(1.5, 7.5), DVec(0.5, 6.5)), List()),
    PolygonShape(List(DVec(-0.5, 2.5), DVec(0.5, -0.5), DVec(1.5, 1.5), DVec(2.5, 3.5), DVec(1.5, 5.5)), List()),
    PolygonShape(List(DVec(0.5, -0.5), DVec(3.5, -1.5), DVec(3.5, 1.5), DVec(2.5, 3.5)), List()),
    PolygonShape(List(DVec(-3.5, -4.5), DVec(-2.5, -5.5), DVec(0.5, -5.5), DVec(-1.5, -4.5)), List()),
    PolygonShape(List(DVec(-1.5, -4.5), DVec(0.5, -5.5), DVec(3.5, -3.5), DVec(0.5, -3.5)), List()),
    PolygonShape(List(DVec(0.5, -3.5), DVec(3.5, -3.5), DVec(4.5, -2.5), DVec(3.5, -1.5), DVec(0.5, -0.5)), List()),
    PolygonShape(List(DVec(0.5, -5.5), DVec(2.5, -5.5), DVec(3.5, -4.5), DVec(3.5, -3.5)), List()),
    PolygonShape(List(DVec(3.5, -4.5), DVec(4.5, -4.5), DVec(4.5, -2.5), DVec(3.5, -3.5)), List())
  )

  val docking_points = List(
    new DockingPoints(DVec(-1.5, 9.5), DVec(1.5, 9.5), this, Some(8), Nil),
    new DockingPoints(DVec(1.5, -5.5), DVec(-1.5, -5.5), this, Some(2), List(
      DVec(2.5, -5.5),
      DVec(3.5, -4.5),
      DVec(4.5, -4.5),
      DVec(4.5, -2.5),
      DVec(3.5, -1.5),
      DVec(3.5, 1.5),
      DVec(1.5, 5.5),
      DVec(1.5, 9.5),
      DVec(-1.5, 9.5),
      DVec(-1.5, 5.5),
      DVec(-3.5, 1.5),
      DVec(-3.5, -1.5),
      DVec(-4.5, -2.5),
      DVec(-4.5, -4.5),
      DVec(-3.5, -4.5),
      DVec(-2.5, -5.5)
    ))
  )

  // миллион ньютонов тяги при расходе 4 килограмма в секунду - это соответствует скорости истечения газов 250 км/сек
  // что в 50 раз выше наивысшего полученного на практике значения для химического топлива: литий/водород/фтор - 5000 м/сек
  val four  = new Engine(4, DVec(-3.5, 0.0),  DVec(1.0, 0.0),  1000000, 1, 4, this)
  val six   = new Engine(6, DVec(3.5, 0.0),   DVec(-1.0, 0.0), 1000000, 1, 4, this)
  val seven = new Engine(7, DVec(-1.5, 8.0),  DVec(1.0, 0.0),  10000,   100, 0.04, this)
  val nine  = new Engine(9, DVec(1.5, 8.0),   DVec(-1.0, 0.0), 10000,   100, 0.04, this)
  val eight = new Engine(8, DVec(0.0, 9.5),   DVec(0.0, -1.0), 1000000, 1, 4, this)
  val one   = new Engine(1, DVec(-4.0, -4.5), DVec(0.0, 1.0),  500000,  1, 4, this)
  val three = new Engine(3, DVec(4.0, -4.5),  DVec(0.0, 1.0),  500000,  1, 4, this)
  val two   = new Engine(2, DVec(0.0, -5.5),  DVec(0.0, 1.0),  1000000, 1, 4, this)

  val engines = List(four, six, seven, nine, eight, two, one, three)

  val engines_by_keycodes_map = Map(
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

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
  private def maxPossiblePowerForLinearMovement(max_power: Double,
                                                force_dir: Double,
                                                mass: Double,
                                                to: Double,
                                                from: Double,
                                                max_diff: Double): (Int, Double) = {
    val percent_seq = {
      // сколько процентов тяги двигателя максимально можем использовать в соответствии с ограничением InterfaceHolder.gSwitcher
      if (!InterfaceHolder.gSwitcher.maxGSet) {
        default_percent_seq
      } else {
        val max_percent = math.min(mass * InterfaceHolder.gSwitcher.maxG * OrbitalKiller.earth.g / max_power * 99, 99).toInt.toDouble
        ((max_percent to 1.0 by -1.0) ++ (0.9 to 0.1 by -0.1)).view
      }
    }

    percent_seq.map {
      case percent =>
        val power = max_power * 0.01 * percent
        val force = force_dir * power
        val acc = force / mass
        (howManyTacts(to, from, acc, base_dt), power, percent)
    }.find {
      case ((tacts, result_to), power, percent) =>
        //println(s"maxPossiblePowerForLinearMovement find: $power, $percent: ${math.abs(to - result_to)}")
        val check = math.abs(to - result_to) < max_diff
        /*if(check) {
          println(s"maxPossiblePowerForLinearMovement = ($tacts, $power, $percent)")
        }*/
        check
    }.map {
      case ((tacts, result_to), power, percent) =>
        (tacts, power)
    }.getOrElse({
      //println("maxPossiblePowerForLinearMovement fallback")
      (correction_check_period, max_power * 0.01)
    })
  }

  private def maxPossiblePowerAndTactsForRotation(max_power: Double,
                                                  force_dir: DVec,
                                                  position: DVec,
                                                  I: Double,
                                                  to: Double,
                                                  from: Double,
                                                  max_diff: Double): (Int, Double) = {
    default_percent_seq.map {
      case percent =>
        val power = max_power * 0.01 * percent
        val torque = (-force_dir * power) */ position
        val ang_acc = (torque / I).toDeg
        (howManyTacts(to, from, ang_acc, base_dt), power, percent)
    }.find {
      case ((tacts, result_to), power, percent) =>
        //println(s"maxPossiblePowerAndTactsForRotation find: $power, $percent: ${math.abs(to - result_to)}")
        val check = math.abs(to - result_to) < max_diff
        /*if(check) {
          println(s"maxPossiblePowerAndTactsForRotation = ($tacts, $power, $percent)")
        }*/
        check
    }.map {
      case ((tacts, result_to), power, percent) =>
        (tacts, power)
    }.getOrElse({
      //println("maxPossiblePowerForRotation fallback")
      (correction_check_period, max_power * 0.1)
    })
  }

  override def preserveAngularVelocity(ang_vel_deg: Double) {
    val difference = angularVelocity - ang_vel_deg
    if (difference > angular_velocity_error) {
      val (tacts, power) = maxPossiblePowerAndTactsForRotation(seven.max_power,
                                                               seven.force_dir,
                                                               seven.position + dock_data.map(_.proxy_ship.coordDiff(index)).getOrElse(DVec.zero),
                                                               dock_data.map(_.proxy_ship.currentState.I).getOrElse(currentState.I),
                                                               ang_vel_deg,
                                                               dock_data.map(_.proxy_ship.currentState.ang_vel).getOrElse(currentState.ang_vel),
                                                               angular_velocity_error)
      seven.power = power
      seven.workTimeTacts = tacts
      seven.active = true
      nine.active = false
    } else if (difference < -angular_velocity_error) {
      val (tacts, power) = maxPossiblePowerAndTactsForRotation(nine.max_power,
                                                               nine.force_dir,
                                                               nine.position + dock_data.map(_.proxy_ship.coordDiff(index)).getOrElse(DVec.zero),
                                                               dock_data.map(_.proxy_ship.currentState.I).getOrElse(currentState.I),
                                                               ang_vel_deg,
                                                               dock_data.map(_.proxy_ship.currentState.ang_vel).getOrElse(currentState.ang_vel),
                                                               angular_velocity_error)
      nine.power = power
      nine.workTimeTacts = tacts
      nine.active = true
      seven.active = false
    }
    last_correction_or_check_moment = OrbitalKiller.tacts
  }

  override def preserveVelocity(need_vel: DVec) {
    val n = DVec(0, 1).rotateDeg(rotation).n
    val p = n.p * (-1)

    val ship_velocity_n = linearVelocity * n // from
    val need_vel_n = need_vel * n // to

    val p_diff = math.sqrt((linear_velocity_error * linear_velocity_error - (ship_velocity_n - need_vel_n) * (ship_velocity_n - need_vel_n)).abs)

    val ship_velocity_p = p * linearVelocity
    val need_vel_p = p * need_vel

    val n_diff = math.sqrt((linear_velocity_error * linear_velocity_error - (ship_velocity_p - need_vel_p) * (ship_velocity_p - need_vel_p)).abs)

    val activate_engines = ArrayBuffer[Engine]()

    if (ship_velocity_n - need_vel_n > n_diff) {
      val (tacts, power) = maxPossiblePowerForLinearMovement(eight.max_power,
                                                             eight.force_dir.y,
                                                             dock_data.map(_.proxy_ship.mass).getOrElse(mass),
                                                             need_vel_n,
                                                             ship_velocity_n,
                                                             n_diff)
      eight.power = power
      /*println("===========================")
      println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
      eight.workTimeTacts = tacts
      activate_engines += eight
    } else if (ship_velocity_n - need_vel_n < -n_diff) {
      val (tacts, power) = maxPossiblePowerForLinearMovement(two.max_power,
                                                             two.force_dir.y,
                                                             dock_data.map(_.proxy_ship.mass).getOrElse(mass),
                                                             need_vel_n,
                                                             ship_velocity_n,
                                                             n_diff)
      two.power = power
      /*println("===========================")
      println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
      two.workTimeTacts = tacts
      activate_engines += two
    }

    if (ship_velocity_p - need_vel_p > p_diff) {
      val (tacts, power) = maxPossiblePowerForLinearMovement(six.max_power,
                                                             six.force_dir.x,
                                                             dock_data.map(_.proxy_ship.mass).getOrElse(mass),
                                                             need_vel_p,
                                                             ship_velocity_p,
                                                             p_diff)
      six.power = power
      /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
      println("===========================")*/
      six.workTimeTacts = tacts
      activate_engines += six
    } else if (ship_velocity_p - need_vel_p < -p_diff) {
      val (tacts, power) = maxPossiblePowerForLinearMovement(four.max_power,
                                                             four.force_dir.x,
                                                             dock_data.map(_.proxy_ship.mass).getOrElse(mass),
                                                             need_vel_p,
                                                             ship_velocity_p,
                                                             p_diff)
      four.power = power
      /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
      println("===========================")*/
      four.workTimeTacts = tacts
      activate_engines += four
    }
    activateOnlyTheseEngines(activate_engines: _*)
    last_correction_or_check_moment = OrbitalKiller.tacts
  }

  override var vertical_speed_msec: Int = 0
  override var horizontal_speed_msec: Int = 0

  private def angleMinDiff(angle1: Double, angle2: Double): Double = {
    val x = math.abs(angle1 - angle2)
    if (x > 180) 360 - x else x
  }

  override def afterStep(time_msec: Long): Unit = {
    super.afterStep(time_msec)
    if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
      if (canDockWithNearestShip && notDocked &&
        (InterfaceHolder.dockingSwitcher.dockingAuto || (InterfaceHolder.dockingSwitcher.dockingManual && InterfaceHolder.dockUndock.needDock))) {
        dock()
        if (isDocked) {
          InterfaceHolder.dockUndock.setDocked()
          InterfaceHolder.dockingSwitcher.setDockingManual()
        }
      }
    }
    if (isDocked && InterfaceHolder.dockUndock.needUndock) {
      undock()
    }
  }

  private def decideSpeedValue(dist: Double): Double = {
    val dist_abs = math.abs(dist)
    // разные значения скоростей для разных ограничений перегрузок - потому что если ограничение на перегрузку, можем просто не успеть
    // затормозить с высокой скорости и врезаться
    val speeds = InterfaceHolder.gSwitcher.maxG match {
      case 1 => (25, 25, 15, 10, 10, 5, 2)
      case 4 => (50, 50, 50, 25, 10, 5, 2)
      case _ => (100, 100, 50, 25, 10, 5, 2)
    }
    val ans = if (dist_abs > 500) speeds._1
    else if (dist_abs > 250) speeds._2
    else if (dist_abs > 125) speeds._3
    else if (dist_abs > 50) speeds._4
    else if (dist_abs > 25) speeds._5
    else if (dist_abs > 10) speeds._6
    else speeds._7
    if (dist >= 0) ans else -ans
  }

  action {
    if (fuelMass <= 0 && flightMode != FreeFlightMode) {
      flightMode = FreeFlightMode
    } else {
      flightMode match {
        case FreeFlightMode => // свободный режим
        case Killrot => // запрет вращения
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
              else flightMode = FreeFlightMode
            } else {
              preserveAngularVelocity(0)
            }
          }
        case RelativeVelocityAligned => // ориентация по траектории
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            val angle = DVec(0, 1).deg360(relativeLinearVelocity)
            if (angleMinDiff(rotation, angle) < angle_error) {
              if (math.abs(angularVelocity) < angular_velocity_error) {
                if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
              } else {
                preserveAngularVelocity(0)
              }
            } else preserveAngle(angle)
          }
        case OppositeRelativeVelocityAligned => // ориентация против траектории
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            val angle = DVec(0, -1).deg360(relativeLinearVelocity)
            if (angleMinDiff(rotation, angle) < angle_error) {
              if (math.abs(angularVelocity) < angular_velocity_error) {
                if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
              } else {
                preserveAngularVelocity(0)
              }
            } else preserveAngle(angle)
          }
        case CirclularOrbit => // выход на орбиту
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              orbitData match {
                case Some(or) =>
                  val ss = satelliteSpeed(coord, linearVelocity, or.planet.coord, or.planet.linearVelocity, or.planet.mass, G)
                  if (linearVelocity.dist(ss) > linear_velocity_error) {
                    preserveVelocity(ss)
                  } else flightMode = FreeFlightMode
                case None =>
                  flightMode = FreeFlightMode
              }
            } else preserveAngularVelocity(0)
          }
        case NearestShipVelocity => // уравнять скорость с ближайшим кораблем, который находится на расстоянии не далее 500 км
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            shipCloser500KmNonMinimized match {
              case Some(s) =>
                if (math.abs(angularVelocity) < angular_velocity_error) {
                  val ss = s.linearVelocity
                  if (linearVelocity.dist(ss) > linear_velocity_error) {
                    preserveVelocity(ss)
                  } else {
                    if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
                    else flightMode = FreeFlightMode
                  }
                } else preserveAngularVelocity(0)
              case None =>
                if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
            }
          }
        case NearestShipAligned => // ориентация на ближайший корабль
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            shipCloser500KmNonMinimized match {
              case Some(os) =>
                val angle = DVec(0, 1).deg360(os.coord - coord)
                if (angleMinDiff(rotation, angle) < angle_error) {
                  if (math.abs(angularVelocity) < angular_velocity_error) {
                    if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
                    else flightMode = FreeFlightMode
                  } else {
                    preserveAngularVelocity(0)
                  }
                } else preserveAngle(angle)
              case None =>
                if (haveSavedFlightMode) restoreFlightModeAndEngineStates()
                else flightMode = FreeFlightMode
            }
          }
        case NearestShipAutoDocking =>
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, correction_check_period)) {
            // 0. если пристыкованы - ничего не делаем
            // 1. определяем ближайшую точку стыковки
            // 2. определяем ориентацию относительно стыковочного узла. Если не соориентированы, ориентируемся параллельно ей
            // 3. смотрим, где мы находимся по вертикали линии стыковки: за центром масс, между центром масс и точкой стыковки, ниже точки стыковки.
            // Если ниже точки стыковки, то все ок, иначе даем импульс, чтобы переместиться ниже.
            // 4. Смотрим, где мы находимся на перпендикуляре линии стыковки. Если требуется, даем импульс и движемся вправо или влево.
            // 5. Если мы на линии стыковки ниже точки стыковки, то даем импульс и движемся наверх.
            // 6. Если мы в зоне стыковки - стыкуемся, и переводим режим полета в "свободный".
            if (isDocked) {
              flightMode = FreeFlightMode
            } else {
              shipCloser500KmNonMinimized match {
                case Some(os) =>
                  InterfaceHolder.dockingSwitcher.setDockingAuto()
                  val dp = docking_points.sortBy(_.curP1.dist2(os.coord)).head
                  val ship_docking_point = dp.curP1 + 0.5 * (dp.curP2 - dp.curP1)
                  os.docking_points.sortBy(osdp => osdp.curP1.dist(ship_docking_point)).headOption match {
                    case Some(osdp) =>
                      if (osdp.curP1.dist(ship_docking_point) > 2000) {
                        // система стыковки начинает работать с расстояния двух километров
                        flightMode = FreeFlightMode
                      } else {
                        val vv1 = (osdp.curP1 - osdp.curP2).n
                        val docking_point = osdp.curP1 + 0.5 * (osdp.curP2 - osdp.curP1)
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
                            val x = (b2 * (A - C) - b1 * (B - D)) / (a1 * b2 - a2 * b1)
                            val y = (a2 * (A - C) - a1 * (B - D)) / (a2 * b1 - a1 * b2)
                            if (y > 0) {
                              // если мы выше точки стыковки
                              // летим вниз пока не окажемся на 20 метров ниже линии стыковки
                              preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y - (-20)))
                            } else {
                              if (math.abs(x) <= 0.2) {
                                // если мы ниже точки стыковки и на линии стыковки
                                // летим стыковаться
                                preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y))
                              } else if (math.abs(x) <= 1) {
                                // если мы ниже точки стыковки и не очень далеко в стороне от линии стыковки
                                // движемся в сторону линии стыковки и в сторону точки стыковки (продолжаем стыковаться)
                                preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y) - vv1 * decideSpeedValue(x))
                              } else {
                                // если мы ниже точки стыковки и далеко от линии стыковки
                                if (y < -20) {
                                  // если мы больше, чем на 20 метров ниже точки стыковки
                                  // летим пока не окажемся на 20 метров ниже линии стыковки и одновременно движемся в сторону линии стыковки
                                  preserveVelocity(os.linearVelocity - docking_dir * decideSpeedValue(y - (-20)) - vv1 * decideSpeedValue(x))
                                } else {
                                  // если мы ниже линии стыковки, но ближе 20 метров
                                  // движемся в сторону линии стыковки, по вертикали свою позицию не меняем
                                  preserveVelocity(os.linearVelocity - vv1 * decideSpeedValue(x))
                                }
                              }
                            }
                          } else {
                            preserveAngularVelocity(0)
                          }
                        } else {
                          if (linearVelocity.dist(os.linearVelocity) > linear_velocity_error) {
                            preserveVelocity(os.linearVelocity)
                          } else {
                            preserveAngle(angle)
                          }
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

  var rockets_enabled = false
  private var left_rocket: Option[Rocket1] = None
  private var right_rocket: Option[Rocket1] = None

  def rocketsStateStr: String = {
    def _lockOn(rocket_pos: DVec): Boolean = {
      shipsCloserXKm(11).exists(os => {
        val our_line = (coord + rocket_pos.rotateDeg(rotation), coord + (rocket_pos + DVec(0, 10000)).rotateDeg(rotation))
        os.draw_points.sliding(2).exists {
          case List(p1, p2) => areLinesIntersect(os.coord + p1.rotateDeg(os.rotation), os.coord + p2.rotateDeg(os.rotation), our_line._1, our_line._2)
        }
      })
    }
    val left_rocket_status = left_rocket match {
      case Some(r) =>
        if (r.isAlive) {
          val vel = msecOrKmsec((linearVelocity - r.linearVelocity) * (coord - r.coord).n)
          val dist = mOrKmOrMKm(coord.dist(r.coord))
          s"[{RED}$rocket_symbol dist=$dist, vel=$vel]"
        } else {
          s"[{DARK_GRAY}$rocket_symbol]"
        }
      case None =>
        if (!_lockOn(DVec(-3, 6.5))) {
          s"[{RED}$rocket_symbol]"
        } else {
          s"[{RED}$rocket_symbol LOCK ON]"
        }
    }
    val right_rocket_status = right_rocket match {
      case Some(r) =>
        if (r.isAlive) {
          val vel = msecOrKmsec((linearVelocity - r.linearVelocity) * (coord - r.coord).n)
          val dist = mOrKmOrMKm(coord.dist(r.coord))
          s"[{RED}$rocket_symbol dist=$dist, vel=$vel]"
        } else {
          s"[{DARK_GRAY}$rocket_symbol]"
        }
      case None =>
        if (left_rocket.isEmpty) {
          if (!_lockOn(DVec(3, 6.5))) {
            s"[{YELLOW}$rocket_symbol]"
          } else {
            s"[{YELLOW}$rocket_symbol LOCK ON]"
          }
        } else if (left_rocket.exists(_.isAlive)) {
          if (!_lockOn(DVec(3, 6.5))) {
            s"[{YELLOW}$rocket_symbol]"
          } else {
            s"[{YELLOW}$rocket_symbol LOCK ON]"
          }
        } else {
          if (!_lockOn(DVec(3, 6.5))) {
            s"[{RED}$rocket_symbol]"
          } else {
            s"[{RED}$rocket_symbol LOCK ON]"
          }
        }
    }
    s"$left_rocket_status $right_rocket_status"
  }

  def launchRocket(): Unit = {
    if (!isDocked && rockets_enabled && (left_rocket.isEmpty || (left_rocket.exists(_.isDead) && right_rocket.isEmpty))) {
      val left_position = left_rocket.isEmpty
      val rocket = new Rocket1(ScageId.nextId,
        init_coord = coord + (if (left_position) DVec(-3, 6.5) else DVec(3, 6.5)).rotateDeg(rotation),
        init_velocity = linearVelocity,
        init_rotation = rotation,
        ship_designer = false
      )

      rocket.two.power = rocket.two.max_power
      rocket.two.workTimeTacts = 63
      rocket.two.active = true

      _payload -= rocket.mass
      if (left_rocket.isEmpty) left_rocket = Some(rocket)
      else if (right_rocket.isEmpty) right_rocket = Some(rocket)
    }
  }

  private val rocket_draw_points = List(
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
    DVec(-1.0, -20.0),
    DVec(1.0, -20.0)
  ).map(_ * 0.1)

  override protected def drawShip(): Unit = {
    if (dockData.isEmpty && !drawMapMode) {
      if (isAlive) {
        openglLocalTransform {
          openglMove(coord - base)
          drawFilledCircle(DVec.zero, 0.3, colorIfPlayerAliveOrRed(GREEN)) // mass center

          if (OrbitalKiller.globalScale >= 0.8) {
            if (!InterfaceHolder.rocketsInfo.isMinimized) {
              left_rocket.foreach(r => {
                if (r.isAlive) {
                  drawArrow(DVec.zero, (r.coord - coord).n * radius, RED)
                }
              })
              right_rocket.foreach(r => {
                if (r.isAlive) {
                  drawArrow(DVec.zero, (r.coord - coord).n * radius, RED)
                }
              })
            }

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

          /*val pa = (earth.coord - coord).n*(coord.dist(earth.coord) - earth.radius) + (earth.coord - coord).p*70000
          val pb = (earth.coord - coord).n*(coord.dist(earth.coord) - earth.radius) + (earth.coord - coord).p*(-70000)
          drawLine(pa, pb, WHITE)*/

          // ниже рисуем aabb и кружочки вокруг формы корабля и отдельных частей формы
          /*val x = currentState.shape.asInstanceOf[PolygonShape]
          drawCircle(DVec.zero, x.radius, WHITE)
          drawRectCentered(DVec.zero, x.radius*2, x.radius*2, WHITE)
          x.convex_parts.foreach(y => {
            drawCircle(y.points_center.rotateDeg(rotation), y.points_radius, WHITE)
            drawRectCentered(y.points_center.rotateDeg(rotation), y.points_radius*2, y.points_radius*2, WHITE)
          })*/

          openglRotateDeg(rotation)

          if (OrbitalKiller.globalScale >= 0.8) {
            if (rockets_enabled) {
              if (left_rocket.isEmpty) {
                openglLocalTransform {
                  openglMove(DVec(-3, 3.5))
                  drawSlidingLines(rocket_draw_points, WHITE)
                }
              }
              if (right_rocket.isEmpty) {
                openglLocalTransform {
                  openglMove(DVec(3, 3.5))
                  drawSlidingLines(rocket_draw_points, WHITE)
                }
              }
            }
          }

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
            if (isDocked) {
              dockData.foreach(d => {
                drawFilledCircle(d.our_dp.p1, 0.3, colorIfPlayerAliveOrRed(GREEN))
                drawFilledCircle(d.our_dp.p2, 0.3, colorIfPlayerAliveOrRed(GREEN))
              })
            } else if (InterfaceHolder.dockingSwitcher.dockingEnabled) {
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
        openglLocalTransform {
          openglMove(coord - base)
          openglRotateDeg(rotation)
          drawSlidingLines(draw_points, colorIfPlayerAliveOrRed(WHITE))
        }
      }
    }
  }
}
