package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable
import collection.mutable.ArrayBuffer
import com.github.dunnololda.simplenet.{State => NetState, _}

class TacticShooterClient(join_game:Option[Int]) extends ScageScreen("Simple Shooter Client") {
  private val client = UdpNetClient(address = host, port = port, ping_timeout= 1000, check_timeout = 5000)

  private val states = mutable.ArrayBuffer[TacticServerData]()
  /*private def optRemoveHeadState:Option[TacticServerData] = {
    if(states.length > 1) Some(states.remove(0))
    else if (states.nonEmpty) Some(states.head)
    else None
  }*/

  private var new_destination:Option[Vec] = None
  private var new_pov:Option[Vec] = None

  private var clear_destinations = false
  private var pov_fixed = false

  private var selected_player = 0

  private var is_game_started = false
  private var is_connected = false
  private val walls = ArrayBuffer[Wall]()
  private var current_state:Option[TacticServerData] = None

  private val menu_items:List[(String, Vec, List[Vec],        () => Any)] = createMenuItems(List(
    ("Продолжить",    Vec(windowWidth/2, windowHeight/2 + 30), () => pauseOff()),
    ("Выход в меню",  Vec(windowWidth/2, windowHeight/2),      () => stop()),
    ("Выход из игры", Vec(windowWidth/2, windowHeight/2-30),   () => stopApp())
  ))

  private def interpolateServerState(third:TacticServerData, second:TacticServerData):TacticServerData = {
    val TacticServerData(third_yours, third_others, third_your_bullets, third_other_bullets, third_receive_moment) = third
    val TacticServerData(second_yours, second_others, second_your_bullets, second_other_bullets, _) = second
    val result_yours = third_yours.map(ty => {
      second_yours.find(_.number == ty.number) match {
        case Some(sy) =>
          ty.copy(
            coord = ty.coord + (sy.coord - ty.coord)*(System.currentTimeMillis() - third_receive_moment)/100f
          )
        case None => ty
      }
    })
    val result_others = third_others.map(to => {
      second_others.find(x => x.id == to.id && x.number == to.number) match {
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
    TacticServerData(result_yours, result_others, result_your_bullets, result_other_bullets, System.currentTimeMillis())
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

  private def checkPausedColor(color:ScageColor):ScageColor = {
    if(on_pause) DARK_GRAY else color
  }

  key(KEY_1, onKeyDown = selected_player = 0)
  key(KEY_2, onKeyDown = selected_player = 1)
  key(KEY_3, onKeyDown = selected_player = 2)
  key(KEY_SPACE, onKeyDown = clear_destinations = true)
  keyIgnorePause(KEY_ESCAPE, onKeyDown = switchPause())

  leftMouseIgnorePause(onBtnDown = m => {
    if(!on_pause) new_destination = Some(m)
    else {
      menu_items.find(x => mouseOnArea(x._3)) match {
        case Some((_, _, _, action)) => action()
        case None =>
      }
    }
  })

  rightMouse(onBtnDown = m => pov_fixed = !pov_fixed)

  mouseWheelDown(m => {
    selected_player -= 1
    if(selected_player < 0) selected_player = 2
  })

  mouseWheelUp(m => {
    selected_player += 1
    if(selected_player > 2) selected_player = 0
  })

  mouseMotion(onMotion = m => {
    if(!pov_fixed) new_pov = Some(m)
  })

  // receive data
  actionIgnorePause(10) {
    client.newEvent {
      case NewUdpServerData(message) =>
        if(message.contains("gamestarted")) is_game_started = true
        if(message.contains("walls")) {
          walls.clear()
          walls ++= serverWalls(message)
        } else {
          val sd = tacticServerData(message, System.currentTimeMillis())
          states += sd
        }
      case UdpServerConnected =>
        is_connected = true
      case UdpServerDisconnected =>
        is_connected = false
        is_game_started = false
    }
  }

  // update state
  actionIgnorePause(10) {
    current_state = optInterpolatedState
  }

  // send data
  actionIgnorePause(50) {
    if(!is_game_started) {
      join_game match {
        case Some(game_id) => client.send(NetState("join" -> game_id))
        case None => client.send(NetState("create" -> true))
      }
    } else {
      if(new_destination.nonEmpty || new_pov.nonEmpty || walls.isEmpty) {
        val builder = NetState.newBuilder
        builder += ("pn", selected_player)
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
  }

  render {
    if(is_connected) {
      current_state match {
        case Some(TacticServerData(yours, others, your_bullets, other_bullets, _)) =>
          yours.foreach(you => {
            if(you.number == selected_player) {
              drawCircle(you.coord, 10, checkPausedColor(YELLOW))
              print(you.number+1, you.coord, checkPausedColor(YELLOW), align = "center")
              you.destinations.foreach(d => drawFilledCircle(d, 3, checkPausedColor(YELLOW)))
              if(you.destinations.length > 0) {
                (you.coord :: you.destinations).sliding(2).foreach {
                  case List(a, b) =>
                    drawLine(a, b, checkPausedColor(YELLOW))
                    print(f"${b.dist(a)/10f}%.2f m", a + (b - a).n * (b.dist(a) * 0.5f), checkPausedColor(YELLOW))
                }
              }
              if(!pov_fixed && !on_pause) {
                val m = mouseCoord
                val pov = (m - you.coord).n
                val pov_point = you.coord + (m - you.coord).n*100f
                drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), checkPausedColor(YELLOW))
                drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), checkPausedColor(YELLOW))
                if(pov_fixed) drawCircle(pov_point, 7, checkPausedColor(YELLOW))
                val pov_area = povTriangle(you.coord, pov, pov_distance, pov_angle)
                drawSlidingLines(pov_area, DARK_GRAY)
              } else {
                val pov_point = you.coord + you.pov*100f
                drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), checkPausedColor(YELLOW))
                drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), checkPausedColor(YELLOW))
                if(pov_fixed) drawCircle(pov_point, 7, checkPausedColor(YELLOW))
                drawSlidingLines(you.pov_area, DARK_GRAY)
              }
              drawCircle(you.coord, audibility_radius, DARK_GRAY)
            } else {
              drawCircle(you.coord, 10, checkPausedColor(GREEN))
              print(you.number+1, you.coord, checkPausedColor(GREEN), align = "center")
              you.destinations.foreach(d => drawFilledCircle(d, 3, checkPausedColor(GREEN)))
              if(you.destinations.length > 0) {
                (you.coord :: you.destinations).sliding(2).foreach {
                  case List(a, b) =>
                    drawLine(a, b, checkPausedColor(GREEN))
                    print(f"${b.dist(a)/10f}%.2f m", a + (b - a).n * (b.dist(a) * 0.5f), checkPausedColor(GREEN))
                }
              }
              val pov_point = you.coord + you.pov*100f
              drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), checkPausedColor(GREEN))
              drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), checkPausedColor(GREEN))
              drawCircle(pov_point, 7, checkPausedColor(GREEN))
              drawSlidingLines(you.pov_area, DARK_GRAY)
              drawCircle(you.coord, audibility_radius, DARK_GRAY)
            }
          })

          others.filter(_.visible).zipWithIndex.foreach {
            case (player, number) =>
              drawCircle(player.coord, 10, checkPausedColor(RED))
              print(number+1, player.coord, checkPausedColor(RED), align = "center")
              val pov_point = player.coord + player.pov*100f
              drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), checkPausedColor(RED))
              drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), checkPausedColor(RED))
              drawSlidingLines(player.pov_area, DARK_GRAY)
              drawCircle(player.coord, audibility_radius, DARK_GRAY)
          }

          your_bullets.foreach(b => {
            val color = if(b.player_number == selected_player) checkPausedColor(YELLOW) else checkPausedColor(GREEN)
            drawRectCentered(b.coord, bullet_size, bullet_size, color)
          })

          other_bullets.foreach(b => {
            drawRectCentered(b.coord, bullet_size, bullet_size, checkPausedColor(RED))
          })

          walls.foreach(wall => {
            drawLine(wall.from, wall.to, checkPausedColor(WHITE))
          })
        case None =>
      }
    }
  }

  interface {
    if(!is_connected) {
      print("Подключаемся к серверу...", windowCenter, DARK_GRAY, align = "center")
    } else {
      current_state match {
        case Some(TacticServerData(yours, others, your_bullets, other_bullets, _)) =>
          val stats_builder = new StringBuilder
          val (your_wins, your_deaths) = yours.foldLeft((0, 0)) {case ((resw, resd), you) => (resw+you.wins, resd+you.deaths)}
          stats_builder append s"{$your_wins : $your_deaths} "
          val pewpew = others.groupBy(x => x.id).values.map(x => x.foldLeft((0, 0)) {case ((resw, resd), y) => (resw+y.wins, resd+y.deaths)})
          pewpew.zipWithIndex.foreach(x => stats_builder append s"${x._2+1} : {${x._1._1} : ${x._1._2}} ")
          print(stats_builder.toString().trim, 20, 20, checkPausedColor(WHITE))
        case None =>
      }
      if(on_pause) {
        menu_items.foreach {
          case (title, coord, _, _) =>
            print(title, coord, WHITE, align = "center")
        }
      }
    }
  }

  dispose {
    client.stop()
  }
}
