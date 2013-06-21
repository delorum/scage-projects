package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable
import collection.mutable.ArrayBuffer
import com.github.dunnololda.simplenet.{State => NetState, _}

object TacticShooterClient extends ScageScreenApp(s"Tactic Shooter v$appVersion", map_width, map_height) {
  private val client = UdpNetClient(address = "fzeulf.netris.ru", port = 10000, ping_timeout= 1000, check_timeout = 5000)

  private val states = mutable.ArrayBuffer[TacticServerData]()
  private def optRemoveHeadState:Option[TacticServerData] = {
    if(states.length > 1) Some(states.remove(0))
    else if (states.nonEmpty) Some(states.head)
    else None
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
  action(100) {
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
          val sd = tacticServerData(message)
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
      optRemoveHeadState match {
        case Some(TacticServerData(you, others, your_bullets, other_bullets)) =>
          drawCircle(you.coord, 10, RED)
          you.destinations.foreach(d => drawFilledCircle(d, 3, YELLOW))
          val pov_point = you.coord + you.pov*pov_distance/2f
          drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), RED)
          drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), RED)
          if(pov_fixed) drawCircle(pov_point, 7, RED)
          drawSlidingLines(you.pov_area, DARK_GRAY)
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
            drawRectCentered(b, bullet_size, bullet_size, RED)
          })

          other_bullets.foreach(b => {
            drawRectCentered(b, bullet_size, bullet_size, WHITE)
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
