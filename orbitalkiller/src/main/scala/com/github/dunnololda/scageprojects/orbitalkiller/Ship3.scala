package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import OrbitalKiller._

class Ship3(
             index:String,
             init_coord:DVec,
             init_velocity:DVec = DVec.dzero,
             init_rotation:Double = 0.0
             ) extends PolygonShip(index, init_coord, init_velocity, init_rotation) {
  val mass = 1000.0

  val points:List[DVec] = List(
    DVec(-10.0, 70.0),
    DVec(10.0, 70.0),
    DVec(10.0, 30.0),
    DVec(50.0, 10.0),
    DVec(10.0, 10.0),
    DVec(10.0, -10.0),
    DVec(50.0, -30.0),
    DVec(10.0, -30.0),
    DVec(10.0, -70.0),
    DVec(-10.0, -70.0),
    DVec(-10.0, -30.0),
    DVec(-50.0, -30.0),
    DVec(-10.0, -10.0),
    DVec(-10.0, 10.0),
    DVec(-50.0, 10.0),
    DVec(-10.0, 30.0)
  )

  val draw_points = (points :+ points.head).map(_.toVec)

  val eight = Engine(position = DVec(0.0, 70.0),    force_dir = DVec(0.0, -1.0), max_power = 1000000, this)
  val two   = Engine(position = DVec(0.0, -70.0),   force_dir = DVec(0.0, 1.0),  max_power = 1000000, this)
  val four  = Engine(position = DVec(-10.0, 0.0),   force_dir = DVec(1.0, 0.0),  max_power = 1000000, this)
  val six   = Engine(position = DVec(10.0, 0.0),    force_dir = DVec(-1.0, 0.0), max_power = 1000000, this)
  val seven = Engine(position = DVec(-40.0, 10.0),  force_dir = DVec(0.0, 1.0),  max_power = 10000, this)
  val nine  = Engine(position = DVec(40.0, 10.0),   force_dir = DVec(0.0, 1.0),  max_power = 10000, this)
  val one   = Engine(position = DVec(-40.0, -30.0), force_dir = DVec(0.0, 1.0),  max_power = 10000, this)
  val three = Engine(position = DVec(40.0, -30.0),  force_dir = DVec(0.0, 1.0),  max_power = 10000, this)

  val engines = List(eight, two, four, six, seven, nine, one, three)

  val engines_mapping = Map(
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

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
    (99 to 1 by -1).find {
      case percent =>
        val power = max_power*0.01*percent
        val force = force_dir*power
        val acc = force / mass
        val (_, result_to) = howManyTacts(to, from, acc, dt)
        math.abs(to - result_to) < max_diff
    }.map(percent => max_power*0.01*percent).getOrElse(max_power*0.01f)
  }
  
  private def maxPossiblePowerForRotation(max_power:Double, force_dir:DVec, position:DVec, I:Double, to:Double, from:Double, max_diff:Double):Double = {
    (99 to 1 by -1).find {
      case percent =>
        val power = max_power*0.01f*percent
        val torque = (-force_dir*power)*/position
        val ang_acc = (torque / I).toDeg
        val (_, result_to) = howManyTacts(to, from, ang_acc, dt)
        math.abs(to - result_to) < max_diff
    }.map(percent => max_power*0.01f*percent).getOrElse(max_power*0.01f)
  }

  private var last_correction_or_check_moment:Long = 0l

  def preserveAngularVelocity(ang_vel_deg:Double) {
    val difference = angularVelocity - ang_vel_deg
    if(difference > 0.01f) {
      val power = maxPossiblePowerForRotation(seven.max_power, seven.force_dir, seven.position, currentState.I, ang_vel_deg, angularVelocity, 0.01f)
      seven.power = power
      eight.power = power
      val ang_acc = (seven.torque / currentState.I).toDeg
      val (tacts, _) = howManyTacts(ang_vel_deg, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(seven, eight)
      seven.worktimeTacts = tacts
      eight.worktimeTacts = tacts
    } else if(difference < -0.01f) {
      val power = maxPossiblePowerForRotation(nine.max_power, nine.force_dir, nine.position, currentState.I, ang_vel_deg, angularVelocity, 0.01f)
      nine.power = power
      eight.power = power
      val ang_acc = (nine.torque / currentState.I).toDeg
      val (tacts, _) = howManyTacts(ang_vel_deg, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(nine, eight)
      nine.worktimeTacts = tacts
      eight.worktimeTacts = tacts
    }
    last_correction_or_check_moment = OrbitalKiller.tacts
  }

  def enterOrbit() {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(body) =>
        val ss = satelliteSpeed(coord, body.coord, body.linearVelocity, body.mass, G)
        val n = DVec(0, 1).rotateDeg(rotation).n
        val p = n.p*(-1)

        val ship_velocity_n = linearVelocity*n  // from
        val ss_n = ss*n                         // to

        if(ship_velocity_n - ss_n > 0.1f) {
          val power = maxPossiblePowerForLinearMovement(eight.max_power, eight.force_dir.y, mass, ss_n, ship_velocity_n, 0.1f)

          eight.power = power
          val acc = (eight.force / mass).y
          val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, dt)
          /*println("===========================")
          println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
          eight.active = true
          eight.worktimeTacts = tacts
        } else if(ship_velocity_n - ss_n < -0.1f) {
          val power = maxPossiblePowerForLinearMovement(two.max_power, two.force_dir.y, mass, ss_n, ship_velocity_n, 0.1f)
          two.power = power
          val acc = (two.force / mass).y
          val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, dt)
          /*println("===========================")
          println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")*/
          two.active = true
          two.worktimeTacts = tacts
        }

        val ship_velocity_p = p*linearVelocity
        val ss_p = p*ss

        if(ship_velocity_p - ss_p > 0.1f) {
          val power = maxPossiblePowerForLinearMovement(six.max_power, six.force_dir.x, mass, ss_p, ship_velocity_p, 0.1f)
          six.power = power
          val acc = (six.force / mass).x
          val (tacts, result_to) = howManyTacts(ss_p, ship_velocity_p, acc, dt)
          /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
          println("===========================")*/
          six.active = true
          six.worktimeTacts = tacts
        } else if(ship_velocity_p - ss_p < -0.1f) {
          val power = maxPossiblePowerForLinearMovement(four.max_power, four.force_dir.x, mass, ss_p, ship_velocity_p, 0.1f)
          four.power = power
          val acc = (four.force / mass).x
          val (tacts, result_to) = howManyTacts(ss_p, ship_velocity_p, acc, dt)
          /*println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
          println("===========================")*/
          four.active = true
          four.worktimeTacts = tacts
        }
      case None =>
    }
    last_correction_or_check_moment = OrbitalKiller.tacts
  }

  action {
    if(allEnginesInactive || OrbitalKiller.tacts - last_correction_or_check_moment > 1000) {
      flightMode match {
        case 1 => // свободный режим
        case 2 => // запрет вращения
          if(math.abs(angularVelocity) < 0.01) flightMode = 1
          else preserveAngularVelocity(0)
        case 3 => // ориентация по осям
          if(math.abs(rotation) < 0.1) flightMode = 2
          else preserveAngle(0)
        case 4 => // ориентация по траектории
          val angle = linearVelocity.mydeg(DVec(0,1))
          if(math.abs(rotation - angle) < 0.1) flightMode = 2
          else preserveAngle(angle)
        case 5 => // ориентация против траектории
          val angle = correctAngle(linearVelocity.mydeg(DVec(0,1)) + 180)
          if(math.abs(rotation - angle) < 0.1) flightMode = 2
          else preserveAngle(angle)
        case 6 => // выход на орбиту
          if(math.abs(angularVelocity) < 0.01) {
            insideGravitationalRadiusOfCelestialBody(coord).foreach {
              case body =>
                val ss = satelliteSpeed(coord, body.coord, body.linearVelocity, body.mass, G)
                if(linearVelocity.dist(ss) > 0.1) {
                  enterOrbit()
                } else flightMode = 1
            }
          } else preserveAngularVelocity(0)
        case _ =>
      }
    }
  }

  render {
    if(!drawMapMode) {
      openglLocalTransform {
        openglMove(coord.toVec)
        drawFilledCircle(Vec.zero, 2, GREEN)                                 // mass center
        drawArrow(Vec.zero, (linearVelocity.n*100).toVec, CYAN)              // current velocity
        drawArrow(Vec.zero, ((earth.coord - coord).n*100).toVec, YELLOW)     // direction to eart
        drawArrow(Vec.zero, ((moon.coord - coord).n*100).toVec, GREEN)       // direction to moon

        openglRotateDeg(rotation.toFloat)
        drawSlidingLines(draw_points, WHITE)

        engines.foreach {
          case e =>
            e.force_dir match {
              case DVec(0, -1) => drawEngine(e, e.position + DVec(0, 2.5f),  10, 5,  is_vertical = false)
              case DVec(0, 1)  => drawEngine(e, e.position + DVec(0, -2.5f), 10, 5,  is_vertical = false)
              case DVec(-1, 0) => drawEngine(e, e.position + DVec(2.5f, 0),  5,  10, is_vertical = true)
              case DVec(1, 0)  => drawEngine(e, e.position + DVec(-2.5f, 0), 5,  10, is_vertical = true)
              case _ =>
            }
        }
      }
    }
  }
}
