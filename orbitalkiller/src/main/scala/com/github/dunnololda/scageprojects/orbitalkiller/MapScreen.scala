package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MapScreen extends ScageScreen("Map Screen") {
  private val scale = 1e-6

  private var _time_mulitplier = 1
  private var disable_interface_drawing = false
  private val trajectory_accuracy = 100

  private val future_trajectory = ArrayBuffer[(Long, List[BodyState])]()
  private val future_trajectory_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()
  private val future_trajectory_capacity = 10000

  private def updateFutureTrajectory() {
    future_trajectory.clear()
    future_trajectory ++= {
      val dt = _time_mulitplier/k*base_dt   // 1/k*base_dt соответствует реальному течению времени
      futureSystemEvolutionFrom(dt, tacts, currentBodyStates, enable_collisions = false)
        .take(future_trajectory_capacity)
    }
    updateFutureTrajectoryMap()
  }

  private def updateFutureTrajectoryMap() {
    future_trajectory_map.clear()
    future_trajectory
      .zipWithIndex
      .withFilter(_._2 % trajectory_accuracy == 0)
      .map(_._1)
      .foreach {
      case (time_moment, body_states) =>
        body_states.foreach {
          case bs =>
            future_trajectory_map.getOrElseUpdate(bs.index, ArrayBuffer[(Long, BodyState)]()) += (time_moment -> bs)
        }
    }
  }

  private def continueFutureTrajectory() {
    val (t, s) = future_trajectory.lastOption.getOrElse((tacts, currentBodyStates))
    val steps = {
      val dt = _time_mulitplier/k*base_dt
      futureSystemEvolutionFrom(dt, t, s, enable_collisions = false)
        .take(future_trajectory_capacity)
    }.toSeq
    future_trajectory ++= steps
    continueFutureTrajectoryMap(steps)
  }

  private def continueFutureTrajectoryMap(steps:Seq[(Long, List[BodyState])]) {
    steps
      .zipWithIndex
      .withFilter(_._2 % trajectory_accuracy == 0)
      .map(_._1)
      .foreach {
      case (time_moment, body_states) =>
        body_states.foreach {
          case bs =>
            future_trajectory_map.getOrElseUpdate(bs.index, ArrayBuffer[(Long, BodyState)]()) += (time_moment -> bs)
        }
    }
  }

  init {
    updateFutureTrajectory()
  }

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    _time_mulitplier += 1
  }, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    if(_time_mulitplier > 1) {
      _time_mulitplier -= 1
    }
  }, onKeyUp = updateFutureTrajectory())

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    if(_time_mulitplier == 1) {
      _time_mulitplier = 50
    } else {
      _time_mulitplier += 50
    }
  }, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    if (_time_mulitplier != 1) {
      _time_mulitplier = 1
    }
  }, onKeyUp = updateFutureTrajectory())

  private var _center = earth.coord.toVec*scale

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center += Vec(5/globalScale, 0)})

  keyIgnorePause(KEY_1, onKeyDown = {
    globalScale = 1
    _center = earth.coord.toVec*scale
  })
  keyIgnorePause(KEY_2, onKeyDown = {
    insideGravitationalRadiusOfCelestialBody(ship.coord) match {
      case Some(planet) =>
        if(planet.index == earth.index) {
          globalScale = 1
          _center = earth.coord.toVec*scale
        }
      case None =>
        globalScale = 1
        _center = earth.coord.toVec*scale
    }
    globalScale = 1
    _center = ship.coord.toVec*scale
  })

  keyIgnorePause(KEY_V, onKeyDown = disable_interface_drawing = !disable_interface_drawing)
  key(KEY_ESCAPE, onKeyDown = stop())
  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01f) {
      if(globalScale > 1) globalScale -= 1
      else if(globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
      if(globalScale < 0.01f) globalScale = 0.01f
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 1000) {
      if(globalScale < 1) globalScale += 0.1f
      else globalScale += 1
    }
    println(globalScale)
  })

  center = _center

  render {
    drawCircle(earth.coord.toVec*scale, (earth.radius*scale).toFloat, WHITE)
    drawCircle(earth.coord.toVec*scale, (earth.gravitational_radius*scale).toFloat, color = DARK_GRAY)

    drawCircle(moon.coord.toVec*scale, (moon.radius*scale).toFloat, WHITE)
    drawCircle(moon.coord.toVec*scale, (moon.gravitational_radius*scale).toFloat, color = DARK_GRAY)

    drawFilledCircle(ship.coord.toVec*scale, (ship.radius*scale).toFloat, WHITE)
    future_trajectory_map.foreach {
      case (index, body_states) =>
        drawSlidingLines(body_states.map(_._2.coord.toVec*scale), color = YELLOW)
    }
  }

  interface {
    if(onPause) print("Пауза", windowCenter, align = "center", color = WHITE)
    //print("F1 - Справка, P - пауза", windowWidth - 20, 20, align = "bottom-right", color = GREEN)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS $fps", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)

    if(!disable_interface_drawing) {
      val heights = (520 to 20 by -20).iterator

      /*print(s"Время: ${timeStr((tacts*base_dt*1000f).toLong)}",
        20, heights.next(), ORANGE)*/
      print(s"Ускорение времени на карте: x${_time_mulitplier}",
        20, heights.next(), ORANGE)
      print(f"Ускорение времени общее: x${timeMultiplier*k}%.2f",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      //print("", 20, heights.next(), ORANGE)

      print(f"Расстояние и скорость относительно Земли: ${mOrKm(ship.coord.dist(earth.coord) - earth.radius)}, ${msecOrKmsec(ship.linearVelocity*(ship.coord - earth.coord).n)}",
        20, heights.next(), ORANGE)
      print(f"Расстояние и скорость относительно Луны: ${mOrKm(ship.coord.dist(moon.coord) - moon.radius)}, ${msecOrKmsec(ship.linearVelocity* (ship.coord - moon.coord).n)}",
        20, heights.next(), ORANGE)
      /*print(s"Расчет траектории: ${if(continue_future_trajectory) "[rактивирован]" else "отключен"}",
        20, heights.next(), ORANGE)*/

      print("", 20, heights.next(), ORANGE)

      print(f"Позиция: ${ship.coord.x}%.2f : ${ship.coord.y}%.2f. Расстояние от точки старта: ${mOrKm(ship.coord.dist(ship_start_position))}",
        20, heights.next(), ORANGE)
      print(f"Линейная скорость: ${msecOrKmsec(ship.linearVelocity.norma)} (velx = ${msecOrKmsec(ship.linearVelocity.x)}, vely = ${msecOrKmsec(ship.linearVelocity.y)})",
        20, heights.next(), ORANGE)
      print(f"Угол: ${ship.rotation}%.2f град",
        20, heights.next(), ORANGE)
      print(f"Угловая скорость: ${ship.angularVelocity}%.2f град/сек",
        20, heights.next(), ORANGE)
      print(s"Параметры орбиты: ${orbitStrInPointWithVelocity(ship.mass, ship.coord, ship.linearVelocity)}",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      print(f"Скорость для спутника в данной точке: ${satelliteSpeedStrInPoint(ship.coord)}",
        20, heights.next(), ORANGE)
      print(f"Скорость убегания в данной точке: ${escapeVelocityStrInPoint(ship.coord)}",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      print(s"Двигательная установка: ${if(ship.engines.exists(_.active)) "[rактивирована]" else "отключена"}",
        20, heights.next(), ORANGE)
      print(s"Мощность и время работы отдельных двигателей:",
        20, heights.next(), ORANGE)
      print(s"${ship.engines.map(e => {
        if(ship.selected_engine.exists(x => x == e)) s"[r${e.powerPercent} % (${e.worktimeTacts} т.)]"
        else s"${e.powerPercent} % (${e.worktimeTacts} т.)"
      }).mkString(", ")}",
        20, heights.next()
        , ORANGE)
      print(f"Линейная скорость в момент отключения двигателей: $linearSpeedStrWhenEnginesOff",
        20, heights.next(), ORANGE)
      print(f"Угловая скорость в момент отключения двигателей: $angularSpeedStrWhenEnginesOff",
        20, heights.next(), ORANGE)
      print(s"Параметры орбиты в момент отключения двигателей: $orbitParametersStrWhenEnginesOff",
        20, heights.next(), ORANGE)
    }
  }
}
