package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, _}
import collection.mutable
import collection.mutable.ArrayBuffer
import scala.Some

object ShooterClient extends ScageScreenApp(s"Simple Shooter v$appVersion", default_window_width, default_window_height) {
  private val client = UdpNetClient(address = "fzeulf.netris.ru", port = 10000, ping_timeout= 1000, check_timeout = 5000)

  private val moves = mutable.HashMap[String, Boolean]("up" -> false, "left" -> false, "down" -> false, "right" -> false)
  private val shoots = ArrayBuffer[Vec]()

  key(KEY_W, onKeyDown = moves("up")    = true, onKeyUp = moves("up")    = false)
  key(KEY_A, onKeyDown = moves("left")  = true, onKeyUp = moves("left")  = false)
  key(KEY_S, onKeyDown = moves("down")  = true, onKeyUp = moves("down")  = false)
  key(KEY_D, onKeyDown = moves("right") = true, onKeyUp = moves("right") = false)

  leftMouse(100, onBtnDown = m => shoots += m)

  // send data
  action(100) {

    if(moves.exists(_._2) || shoots.nonEmpty || map.walls.isEmpty) {
      val builder = ArrayBuffer[(String, Any)]()
      builder ++= moves.filter(_._2)
      if(shoots.nonEmpty) {
        builder += ("shoots" -> shoots.map(v => NetState("x" -> v.x, "y" -> v.y)).toList)
      }
      if(map.isEmpty) {
        builder += ("sendmap" -> true)
      }
      val state = NetState(builder:_*)
      client.send(state)
      shoots.clear()
    }
  }

  private val states = mutable.ArrayBuffer[ServerData]()
  private def optRemoveHeadState:Option[ServerData] = {
    if(states.length > 1) Some(states.remove(0))
    else if (states.nonEmpty) Some(states.head)
    else None
  }

  private var is_connected = false

  // receive data
  action(10) {
    client.newEvent {
      case NewUdpServerData(message) =>
        //println(message.toJsonString)
        if(message.contains("walls")) {
          map = gameMap(message.value[NetState]("map").get)
        } else {
          val sd = serverData(message)
          //println(sd)
          states += sd
        }
      case UdpServerConnected => is_connected = true
      case UdpServerDisconnected => is_connected = false
    }
  }

  // update state
  /*action(10) {

  }*/

  private var map = GameMap()

  render {
    if(!is_connected) {
      print("Connecting to Server...", windowCenter, DARK_GRAY, align = "center")
    } else {
      optRemoveHeadState match {
        case Some(ServerData(you, others, your_bullets, other_bullets)) =>
          drawCircle(you.coord, human_size/2, RED)
          others.filter(_.visible).zipWithIndex.foreach(c => {
            drawCircle(c._1.coord, human_size/2, WHITE)
            print(c._2+1, c._1.coord, WHITE, align = "center")
          })
          your_bullets.foreach(b => {
            drawRectCentered(b, bullet_size, bullet_size, RED)
          })
          other_bullets.foreach(b => {
            drawRectCentered(b, bullet_size, bullet_size, WHITE)
          })

          map.walls.filter(w => isWallVisible(w, you.coord, map.walls.filterNot(ow => ow == w))).foreach(wall => {
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
