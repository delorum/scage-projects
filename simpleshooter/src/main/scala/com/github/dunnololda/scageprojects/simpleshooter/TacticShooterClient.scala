package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable
import collection.mutable.ArrayBuffer
import com.github.dunnololda.simplenet.{State => NetState, _}

object TacticShooterClient extends ScageScreenApp(s"Tactic Shooter v$appVersion", map_width, map_height) {
  private val client = UdpNetClient(address = "localhost", port = 10000, ping_timeout= 1000, check_timeout = 5000)

  private val states = mutable.ArrayBuffer[TacticServerData]()
  private def optRemoveHeadState:Option[TacticServerData] = {
    if(states.length > 1) Some(states.remove(0))
    else if (states.nonEmpty) Some(states.head)
    else None
  }

  private def interpolateServerState(third:TacticServerData, second:TacticServerData):TacticServerData = {
    val TacticServerData(third_you, third_others, third_your_bullets, third_other_bullets, third_receive_moment) = third
    val TacticServerData(second_you, second_others, second_your_bullets, second_other_bullets, _) = second
    val result_you = third_you.copy(
      coord = third_you.coord + (second_you.coord - third_you.coord)*(System.currentTimeMillis() - third_receive_moment)/100f,
      pov =  third_you.pov + (second_you.pov - third_you.pov)*(System.currentTimeMillis() - third_receive_moment)/100f,
      pov_area = third_you.pov_area.zip(second_you.pov_area).map {
        case (tpp, spp) => tpp + (spp - tpp)*(System.currentTimeMillis() - third_receive_moment)/100f
      }
    )
    val result_others = third_others.map(to => {
      second_others.find(_.id == to.id) match {
        case Some(so) =>
          to.copy(
            coord = to.coord + (so.coord - to.coord)*(System.currentTimeMillis() - third_receive_moment)/100f
          )
        case None => to
      }
    })
    val result_your_bullets = third_your_bullets.map(tb => {
      second_your_bullets.find(_.id == tb.id) match {
        case Some(sb) =>
          tb.copy(
            coord = tb.coord + (sb.coord - tb.coord)*(System.currentTimeMillis() - third_receive_moment)/100f
          )
        case None => tb
      }
    })
    val result_other_bullets = third_other_bullets.map(tb => {
      second_other_bullets.find(_.id == tb.id) match {
        case Some(sb) =>
          tb.copy(
            coord = tb.coord + (sb.coord - tb.coord)*(System.currentTimeMillis() - third_receive_moment)/100f
          )
        case None => tb
      }
    })
    TacticServerData(result_you, result_others, result_your_bullets, result_other_bullets, System.currentTimeMillis())
  }

  private def optInterpolatedState:Option[TacticServerData] = {
    if(states.length >= 3) {
      val third = states(states.length-3)
      val second = states(states.length-2)
      states.remove(0)
      Some(interpolateServerState(third, second))
    } else if (states.length >= 2) {
      val third = states(states.length-2)
      val second = states(states.length-1)
      Some(interpolateServerState(third, second))
    } else None
  }

  private var new_destination:Option[Vec] = None
  private var new_pov:Option[Vec] = None

  private var clear_destinations = false
  private var pov_fixed = false

  leftMouse(onBtnDown = m => {
    new_destination = Some(m)
  })

  rightMouse(onBtnDown = m => pov_fixed = !pov_fixed)

  mouseMotion(onMotion = m => {
    if(!pov_fixed) new_pov = Some(m)
  })

  key(KEY_SPACE, onKeyDown = clear_destinations = true)

  // send data
  action(50) {
    if(new_destination.nonEmpty || new_pov.nonEmpty || walls.isEmpty) {
      val builder = NetState.newBuilder
      new_destination.foreach(nd => {
        builder += ("d", NetState("x" -> nd.x, "y" -> nd.y))
        new_destination = None
      })
      new_pov.foreach(nd => {
        builder += ("pov", NetState("x" -> nd.x, "y" -> nd.y))
        new_pov = None
      })
      if(walls.isEmpty) builder += ("sendmap", true)
      if(clear_destinations) {
        builder += ("cleardest", true)
        clear_destinations = false
      }
      client.send(builder.toState)
    }
  }

  private var is_connected = false

  // receive data
  action(10) {
    client.newEvent {
      case NewUdpServerData(message) =>
        if(message.contains("walls")) {
          walls.clear()
          walls ++= serverWalls(message)
        } else {
          val sd = tacticServerData(message, System.currentTimeMillis())
          states += sd
        }
      case UdpServerConnected => is_connected = true
      case UdpServerDisconnected => is_connected = false
    }
  }

  private val walls = ArrayBuffer[Wall]()

  render {
    if(!is_connected) {
      print("Connecting to Server...", windowCenter, DARK_GRAY, align = "center")
    } else {
      optInterpolatedState match {
        case Some(TacticServerData(you, others, your_bullets, other_bullets, _)) =>
          drawCircle(you.coord, 10, RED)
          you.destinations.foreach(d => drawFilledCircle(d, 3, YELLOW))
          if(you.destinations.length > 1) drawSlidingLines(you.destinations, YELLOW)
          if(!pov_fixed) {
            val m = mouseCoord
            val pov = (m - you.coord).n
            val pov_point = you.coord + (m - you.coord).n*pov_distance/2f
            drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), RED)
            drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), RED)
            if(pov_fixed) drawCircle(pov_point, 7, RED)
            val pov_area = povTriangle(you.coord, pov, pov_distance, pov_angle)
            drawSlidingLines(pov_area, DARK_GRAY)
          } else {
            val pov_point = you.coord + you.pov*pov_distance/2f
            drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), RED)
            drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), RED)
            if(pov_fixed) drawCircle(pov_point, 7, RED)
            drawSlidingLines(you.pov_area, DARK_GRAY)
          }
          drawCircle(you.coord, audibility_radius, DARK_GRAY)

          others.filter(_.visible).zipWithIndex.foreach {
            case (player, number) =>
              drawCircle(player.coord, 10, WHITE)
              print(number+1, player.coord, WHITE, align = "center")
              val pov_point = player.coord + player.pov*pov_distance/2f
              drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), RED)
              drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), RED)
              drawSlidingLines(player.pov_area, DARK_GRAY)
              drawCircle(player.coord, audibility_radius, DARK_GRAY)
          }

          your_bullets.foreach(b => {
            drawRectCentered(b.coord, bullet_size, bullet_size, RED)
          })

          other_bullets.foreach(b => {
            drawRectCentered(b.coord, bullet_size, bullet_size, WHITE)
          })

          walls.foreach(wall => {
            drawLine(wall.from, wall.to, WHITE)
          })

          val other_stats = others.zipWithIndex.map(c => s"${c._2+1} : {${c._1.health} : ${c._1.wins} : ${c._1.deaths}}").mkString(" ")
          print(s"[r{${you.health} : ${you.wins} : ${you.deaths}}] "+other_stats, 20, 20, WHITE)
        case None =>
      }
    }
  }

  dispose {
    client.stop()
  }
}
