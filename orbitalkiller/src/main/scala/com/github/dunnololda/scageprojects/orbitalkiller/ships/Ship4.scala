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
  def mass:Double = _payload + _fuel_mass
  private val _payload:Double = 5*1000
  private var _fuel_mass:Double = 5*1000
  override def fuelMass: Double = _fuel_mass

  override def fuelMass_=(m: Double): Unit = {
    _fuel_mass = m
  }

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

  override val convex_parts = List(
    PolygonShape(List(DVec(-3.5, -3.5), DVec(3.5, -3.5), DVec(3.5, 2.5), DVec(-3.5, 2.5)), Nil),
    PolygonShape(List(DVec(-3.5, 2.5), DVec(3.5, 2.5), DVec(1.5, 6.5), DVec(-1.5, 6.5)), Nil),
    PolygonShape(List(DVec(-1.5, 10.5), DVec(-1.5, 6.5), DVec(1.5, 6.5), DVec(1.5, 10.5)), Nil)
  )

  override val wreck_parts = List(
    PolygonShape(List(DVec(-3.5, -3.5), DVec(-0.5, -3.5), DVec(-0.5, -0.5), DVec(-3.5, -0.5)), Nil),
    PolygonShape(List(DVec(-0.5, -3.5), DVec(3.5, -3.5), DVec(3.5, -0.5), DVec(-0.5, -0.5)), Nil),
    PolygonShape(List(DVec(-3.5, 2.5), DVec(-3.5, -0.5), DVec(0.5, -0.5), DVec(0.5, 2.5)), Nil),
    PolygonShape(List(DVec(0.5, -0.5), DVec(3.5, -0.5), DVec(3.5, 2.5), DVec(0.5, 2.5)), Nil),
    PolygonShape(List(DVec(-3.5, 2.5), DVec(-0.5, 2.5), DVec(-0.5, 6.5), DVec(-1.5, 6.5)), Nil),
    PolygonShape(List(DVec(-0.5, 2.5), DVec(3.5, 2.5), DVec(1.5, 6.5), DVec(-0.5, 6.5)), Nil),
    PolygonShape(List(DVec(-1.5, 6.5), DVec(1.5, 6.5), DVec(1.5, 8.5), DVec(-1.5, 8.5)), Nil),
    PolygonShape(List(DVec(-1.5, 8.5), DVec(1.5, 8.5), DVec(1.5, 10.5), DVec(-1.5, 10.5)), Nil)
  )

  val draw_points = points :+ points.head

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

  private def howManyTacts(to:Double, from:Double, a:Double, dt:Double):(Int, Double) = {
    val tacts = ((to - from)/(a*dt)).toInt + 1
    val result_to = from + tacts*a*dt
    //println(s"$from -> $to : $result_to : $tacts")
    (tacts, result_to)
    /*if(a == 0) tacts
    else if(a > 0) {
      if(from >= to) tacts
      else howManyTacts(to, from + a*dt, a, dt, tacts+1)
    } else {
      if(from <= to) tacts
      else howManyTacts(to, from + a*dt, a, dt, tacts+1)
    }*/
  }

  private def maxPossiblePowerForLinearMovement(max_power:Double, force_dir:Double, mass:Double, to:Double, from:Double, max_diff:Double):Double = {
    /*(99.99 to 0.01 by -0.01).map {
      case percent =>
        val power = max_power*0.01*percent
        val force = force_dir*power
        val acc = force / mass
        val (_, result_to) = howManyTacts(to, from, acc, dt)
        (percent, math.abs(to - result_to))
    }.filter(_._2 < max_diff).sortBy(_._2).headOption.map(_._1*max_power*0.01).getOrElse(max_power*0.01)*/

    (99 to 1 by -1).find {
      case percent =>
        val power = max_power*0.01*percent
        val force = force_dir*power
        val acc = force / mass
        val (_, result_to) = howManyTacts(to, from, acc, dt)
        math.abs(to - result_to) < max_diff
    }.map(percent => max_power*0.01*percent).getOrElse(max_power*0.01)

    /*(max_power to 1 by -1).find {
      case power =>
        val force = force_dir*power
        val acc = force / mass
        val (_, result_to) = howManyTacts(to, from, acc, dt)
        math.abs(to - result_to) < max_diff
    }.getOrElse(max_power*0.01)*/
  }

  private def maxPossiblePowerForRotation(max_power:Double, force_dir:DVec, position:DVec, I:Double, to:Double, from:Double, max_diff:Double):Double = {
    (99 to 1 by -1).find {
      case percent =>
        val power = max_power*0.01*percent
        val torque = (-force_dir*power)*/position
        val ang_acc = (torque / I).toDeg
        val (_, result_to) = howManyTacts(to, from, ang_acc, dt)
        math.abs(to - result_to) < max_diff
    }.map(percent => max_power*0.01*percent).getOrElse(max_power*0.01)
  }

  override def preserveAngularVelocity(ang_vel_deg: Double) {
    val difference = angularVelocity - ang_vel_deg
    if(difference > angular_velocity_error) {
      val power = maxPossiblePowerForRotation(seven.max_power, seven.force_dir, seven.position, currentState.I, ang_vel_deg, angularVelocity, angular_velocity_error)
      seven.power = power
      //six.power = power
      val ang_acc = (seven.torque / currentState.I).toDeg
      val (tacts, _) = howManyTacts(ang_vel_deg, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(seven/*, six*/)
      seven.workTimeTacts = tacts
      //six.workTimeTacts = tacts
    } else if(difference < -angular_velocity_error) {
      val power = maxPossiblePowerForRotation(nine.max_power, nine.force_dir, nine.position, currentState.I, ang_vel_deg, angularVelocity, angular_velocity_error)
      nine.power = power
      //four.power = power
      val ang_acc = (nine.torque / currentState.I).toDeg
      val (tacts, _) = howManyTacts(ang_vel_deg, angularVelocity, ang_acc, dt)
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
      val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, dt)
      /*println("===========================")
      println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
      eight.workTimeTacts = tacts
      activate_engines += eight
    } else if(ship_velocity_n - ss_n < -linear_velocity_error) {
      val power = maxPossiblePowerForLinearMovement(two.max_power, two.force_dir.y, mass, ss_n, ship_velocity_n, linear_velocity_error)
      two.power = power
      val acc = (two.force / mass).y
      val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, dt)
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
      val (tacts, _) = howManyTacts(ss_p, ship_velocity_p, acc, dt)
      /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
      println("===========================")*/
      six.workTimeTacts = tacts
      activate_engines += six
    } else if(ship_velocity_p - ss_p < -linear_velocity_error) {
      val power = maxPossiblePowerForLinearMovement(four.max_power, four.force_dir.x, mass, ss_p, ship_velocity_p, linear_velocity_error)
      four.power = power
      val acc = (four.force / mass).x
      val (tacts, _) = howManyTacts(ss_p, ship_velocity_p, acc, dt)
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

  action {
    if(fuelMass <= 0 && flightMode != Free) {
      flightMode = Free
    } else {
      flightMode match {
        case Free => // свободный режим
        case Killrot => // запрет вращения
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              if(haveSavedFlightMode) restoreFlightModeAndEngineStates()
              else flightMode = Free
            } else {
              preserveAngularVelocity(0)
            }
          }
        case AxisAligned => // ориентация по осям
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            if (angleMinDiff(rotation, 0) < angle_error) flightMode = Killrot
            else preserveAngle(0)
          }
        case VelocityAligned => // ориентация по траектории
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            val angle = DVec(0, 1).deg360(relativeLinearVelocity)
            if (angleMinDiff(rotation, angle) < angle_error) flightMode = Killrot
            else preserveAngle(angle)
          }
        case OppositeVelocityAligned => // ориентация против траектории
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            val angle = DVec(0, -1).deg360(relativeLinearVelocity)
            if (angleMinDiff(rotation, angle) < angle_error) flightMode = Killrot
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
                  } else flightMode = Free
                case None =>
                  flightMode = Free
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
                    flightMode = Free
                  }
                case None =>
                  flightMode = Free
              }
            } else preserveAngularVelocity(0)
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
                  } else {
                  }
                }
              }
            case None =>
          }
        case AbsoluteStop => // остановиться
          if (allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment >= math.min(OrbitalKiller.tacts, 1000)) {
            if (math.abs(angularVelocity) < angular_velocity_error) {
              if (linearVelocity.dist(DVec.dzero) > linear_velocity_error) {
                preserveVelocity(DVec.dzero)
              } else flightMode = Free
            } else preserveAngularVelocity(0)
          }
        case _ =>
      }
    }
  }

  render {
    /*if(renderingEnabled) {*/
      if(!drawMapMode) {
        if(pilotIsAlive) {
          openglLocalTransform {
            openglMove(coord - base)
            drawFilledCircle(DVec.zero, 0.3, colorIfAliveOrRed(GREEN)) // mass center

            if (OrbitalKiller.globalScale >= 0.8) {
              if (!InterfaceHolder.linearVelocityInfo.isMinimized) {

                // current velocity
                drawArrow(DVec.zero, linearVelocity.n * 20, colorIfAliveOrRed(BLUE))
                drawArrow(DVec.zero, relativeLinearVelocity.n * 20, colorIfAliveOrRed(InterfaceHolder.linearVelocityInfo.color))
              }
              //drawArrow(DVec.zero, linearAcceleration.n * 100, ORANGE)        // current acceleration
              if (!InterfaceHolder.sunRelativeInfo.isMinimized) {
                // direction to earth
                drawArrow(Vec.zero, (sun.coord - coord).n * 20, colorIfAliveOrRed(InterfaceHolder.sunRelativeInfo.color))
              }
              if (!InterfaceHolder.earthRelativeInfo.isMinimized) {
                // direction to earth
                drawArrow(Vec.zero, (earth.coord - coord).n * 20, colorIfAliveOrRed(InterfaceHolder.earthRelativeInfo.color))
              }
              if (!InterfaceHolder.moonRelativeInfo.isMinimized) {
                // direction to moon
                drawArrow(Vec.zero, (moon.coord - coord).n * 20, colorIfAliveOrRed(InterfaceHolder.moonRelativeInfo.color))
              }
              if (!InterfaceHolder.nearestShipInfo.isMinimized) {
                // direction to nearest ship
                otherShipsNear.headOption.foreach(x => {
                  drawArrow(Vec.zero, (x.coord - coord).n * 20, colorIfAliveOrRed(InterfaceHolder.nearestShipInfo.color))
                })
              }
            }

            /*val pa = (earth.coord - coord).n*(coord.dist(earth.coord) - earth.radius) + (earth.coord - coord).p*70000
          val pb = (earth.coord - coord).n*(coord.dist(earth.coord) - earth.radius) + (earth.coord - coord).p*(-70000)
          drawLine(pa, pb, WHITE)*/

            openglRotateDeg(rotation)
            drawSlidingLines(draw_points, colorIfAliveOrRed(WHITE))

            engines.foreach {
              case e => drawEngine(e)
            }
          }
        } else {
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
              drawSlidingLines(mbs_points :+ mbs_points.head, colorIfAliveOrRed(RED))
            }
          })
        }
      }
    /*}*/
  }
}
