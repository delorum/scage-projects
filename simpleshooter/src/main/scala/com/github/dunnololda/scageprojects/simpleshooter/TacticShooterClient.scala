package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable
import com.github.dunnololda.simplenet.{State => NetState, _}

class TacticShooterClient(join_game:Option[Int]) extends ScageScreen("Simple Shooter Client") {
  private val client = UdpNetClient(address = host, port = port, ping_timeout= 1000, check_timeout = 5000)

  private val states = mutable.ArrayBuffer[TacticServerData]()

  private var game_stats:Option[GameStats] = None

  private val builder = NetState.newBuilder

  private var new_destination:Option[Vec] = None
  private var new_pov:Option[Vec] = None

  private var clear_destinations = false
  private var pov_fixed = false

  private var selected_player = 0

  private var is_game_started = false
  private var is_connected = false
  private var map = GameMap()
  private var current_state:Option[TacticServerData] = None

  private val fire_toggles = mutable.HashMap[Int, Int]()
  private def fireToggle = fire_toggles.getOrElseUpdate(selected_player, 0)
  private def fireToggle_=(new_ft:Int) {fire_toggles(selected_player) = new_ft}

  private var send_fire_toggle:Option[Int] = None // Some(0), Some(1), Some(2): no fire, single fire, rapid fire

  private val menu_items = createMenuItems(List(
    ("Продолжить",    () => Vec(windowWidth/2, windowHeight/2 + 30), WHITE, () => pauseOff()),
    ("Выход в меню",  () => Vec(windowWidth/2, windowHeight/2),      WHITE, () => stop()),
    ("Выход из игры", () => Vec(windowWidth/2, windowHeight/2-30),   WHITE, () => stopApp())
  ))

  private var selected_menu_item:Option[Int] = None
  private def menuItemColor(idx:Int, color:ScageColor):ScageColor = {
    selected_menu_item match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => color
    }
  }

  private var render_mouse = scaledCoord(mouseCoord)
  private val number_place = Vec(1, 1).n*human_size*2

  private var _center = Vec.zero
  private var dir = Vec.zero

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

  private def ourPlayerColor(player:TacticClientPlayer, is_selected:Boolean):ScageColor = {
    if(on_pause) DARK_GRAY
    else if(player.isDead) GRAY
    else if(is_selected) YELLOW
    else GREEN
  }

  private def enemyPlayerColor(player:TacticClientPlayer):ScageColor = {
    if(on_pause) DARK_GRAY
    else if(player.isDead) GRAY
    else RED
  }

  private def inputChanged:Boolean = {
    new_destination.nonEmpty ||
    new_pov.nonEmpty ||
    map.walls.isEmpty ||
    clear_destinations ||
    send_fire_toggle.nonEmpty ||
    builder.nonEmpty
  }

  private def selectPlayer(number:Int) {
    if(selected_player == number) {
      _center = current_state.map(x => x.yours(selected_player).coord).getOrElse(_center)
    } else selected_player = number
  }

  key(KEY_1, onKeyDown = selectPlayer(0))
  key(KEY_2, onKeyDown = selectPlayer(1))
  key(KEY_3, onKeyDown = selectPlayer(2))
  key(KEY_SPACE, onKeyDown = clear_destinations = true)
  keyIgnorePause(KEY_ESCAPE, onKeyDown = switchPause())

  key(KEY_LSHIFT, onKeyDown = {
    if(fireToggle > 0) {
      fireToggle -= 1
      send_fire_toggle = Some(fireToggle)
    }
  })

  key(KEY_LCONTROL, onKeyDown = {
    if(fireToggle < 2) {
      fireToggle += 1
      send_fire_toggle = Some(fireToggle)
    }
  })

  key(KEY_W, 10, onKeyDown = dir += Vec(0, 1))
  key(KEY_A, 10, onKeyDown = dir += Vec(-1, 0))
  key(KEY_S, 10, onKeyDown = dir += Vec(0, -1))
  key(KEY_D, 10, onKeyDown = dir += Vec(1, 0))

  leftMouseIgnorePause(
    onBtnDown = m => {
      if(!on_pause) {
        val sm = scaledCoord(m)
        if(isCoordInsideMapBorders(sm)) {
          new_destination = Some(sm)
        }
      } else {
        menu_items.zipWithIndex.find(x => mouseOnArea(x._1._3())) match {
          case Some((pewpew, idx)) => selected_menu_item = Some(idx)
          case None =>
        }
      }
    },
    onBtnUp = m => {
      if(on_pause) {
        selected_menu_item.foreach(idx => menu_items(idx)._5())
        selected_menu_item = None
      }
    }
  )

  rightMouse(onBtnDown = m => pov_fixed = !pov_fixed)

  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 0.1f) {
      if(globalScale > 1) globalScale -= 1
      else globalScale -= 0.1f
    }
  })

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 3) globalScale += 1
  })

  mouseMotion(onMotion = m => {
    if(!pov_fixed) new_pov = Some(scaledCoord(m))
  })

  // receive data
  actionIgnorePause(10) {
    client.newEvent {
      case NewUdpServerData(message) =>
        //println(message.toJsonString)
        if(message.contains("gamestarted")) is_game_started = true
        if(message.contains("fire_toggle_set")) send_fire_toggle = None
        if(message.contains("dests_cleared")) clear_destinations = false
        message.value[NetState]("map").foreach(m => map = gameMap(m))
        message.value[NetState]("gs").foreach(m => {
          game_stats = Some(gameStats(m))
          builder += ("game_stats_update_received" -> true)
        })
        controlPointInfos(message).foreach {
          case ControlPointInfo(number, team, control_time) => map.control_points.get(number).foreach(x => {
            x.team = team
            x.control_start_time = control_time
          })
        }
        val sd = tacticServerData(message, System.currentTimeMillis())
        states += sd
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
    if(dir.notZero) {
      val new_center = _center + dir.n*human_size/2
      if(isCoordInsideMapBorders(new_center)) _center = new_center
      dir = Vec.zero
    }
  }

  action {
    if(current_state.nonEmpty) {
      selectPlayer(selected_player)
      deleteSelf()
    }
  }

  // send data
  actionIgnorePause(50) {
    if(!is_game_started) {
      join_game match {
        case Some(game_id) => client.send(NetState("join" -> game_id))
        case None => client.send(NetState("create" -> true))
      }
    } else {
      if(inputChanged) {
        builder += ("pn" -> selected_player)
        new_destination.foreach(nd => {
          builder += ("d" -> NetState("x" -> nd.x, "y" -> nd.y))
          new_destination = None
        })
        new_pov.foreach(nd => {
          builder += ("pov" -> NetState("x" -> nd.x, "y" -> nd.y))
          new_pov = None
        })
        if(map.isEmpty) builder += ("sendmap" -> true)
        if(clear_destinations) {
          builder += ("cleardest" -> true)
        }
        send_fire_toggle.foreach(ft => builder += ("ft" -> ft))
        client.send(builder.toState)
        builder.clear()
      }
    }
  }

  center = /*current_state.map {
    case TacticServerData(yours, others, your_bullets, other_bullets, _) =>
      yours(selected_player).coord
  }.getOrElse(Vec.zero)*/_center

  render {
    if(is_connected && is_game_started) {
      current_state match {
        case Some(TacticServerData(yours, others, your_bullets, other_bullets, _)) =>
          val you = yours(selected_player)
          val walls_color = checkPausedColor(WHITE)
          map.walls.foreach(wall => {
            drawLine(wall.from, wall.to, walls_color)
            drawCircle(wall.from, near_wall_area, GRAY)
            drawCircle(wall.to, near_wall_area, GRAY)
          })
          val safe_zones_color = checkPausedColor(GREEN)
          map.safe_zones.foreach(sz => {
            drawSlidingLines(sz, safe_zones_color)
          })
          map.control_points.foreach(cp => {
            drawSlidingLines(cp._2.area, checkPausedColor(cp._2.controlPointColor(you.team)))
          })
          val edges_color = checkPausedColor(GRAY)
          drawSlidingLines(map_edges, edges_color)

          yours.foreach(you => {
            if(you.number == selected_player) {
              val color = ourPlayerColor(you, is_selected = true)
              drawCircle(you.coord, 10, color)
              print(s"${you.number_in_team+1}.${you.number+1} ${if(you.is_reloading) "перезарядка" else you.bullets}", you.coord+number_place, max_font_size/globalScale, color)
              you.destinations.foreach(d => drawFilledCircle(d, 3, color))
              if(you.destinations.length > 0) {
                (you.coord :: you.destinations).sliding(2).foreach {
                  case List(a, b) =>
                    drawLine(a, b, color)
                    print(f"${b.dist(a)/10f}%.2f m", a + (b - a).n * (b.dist(a) * 0.5f), max_font_size/globalScale, color)
                }
              }
              if(!on_pause) {
                render_mouse = scaledCoord(mouseCoord)
              }
              if(!pov_fixed && !on_pause) {
                val pov = (render_mouse - you.coord).n
                val pov_point = you.coord + (render_mouse - you.coord).n*100f
                drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), color)
                drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), color)
                if(pov_fixed) drawCircle(pov_point, 7, color)
                val pov_point1 = you.coord + pov.rotateDeg(pov_angle) * pov_distance
                val pov_point2 = you.coord + pov.rotateDeg(-pov_angle) * pov_distance
                drawLine(you.coord, pov_point1, DARK_GRAY)
                drawLine(you.coord, pov_point2, DARK_GRAY)
              } else {
                val pov_point = you.coord + you.pov*100f
                drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), color)
                drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), color)
                if(pov_fixed) drawCircle(pov_point, 7, color)
                val pov_point1 = you.coord + you.pov.rotateDeg(pov_angle) * pov_distance
                val pov_point2 = you.coord + you.pov.rotateDeg(-pov_angle) * pov_distance
                drawLine(you.coord, pov_point1, DARK_GRAY)
                drawLine(you.coord, pov_point2, DARK_GRAY)
              }
              drawCircle(you.coord, human_audibility_radius, DARK_GRAY)
              drawCircle(you.coord, bullet_audibility_radius, DARK_GRAY)
              drawLine(you.coord, render_mouse, DARK_GRAY)
              val r = render_mouse.dist(you.coord)
              print(f"${r/10f}%.2f m", render_mouse, max_font_size/globalScale, DARK_GRAY)
            } else {
              val color = ourPlayerColor(you, is_selected = false)
              drawCircle(you.coord, 10, color)
              print(s"${you.number_in_team+1}.${you.number+1}  ${if(you.is_reloading) "перезарядка" else you.bullets}", you.coord+number_place, max_font_size/globalScale, color)
              you.destinations.foreach(d => drawFilledCircle(d, 3, color))
              if(you.destinations.length > 0) {
                (you.coord :: you.destinations).sliding(2).foreach {
                  case List(a, b) =>
                    drawLine(a, b, color)
                    print(f"${b.dist(a)/10f}%.2f m", a + (b - a).n * (b.dist(a) * 0.5f), max_font_size/globalScale, color)
                }
              }
              val pov_point = you.coord + you.pov*100f
              drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), color)
              drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), color)
              drawCircle(pov_point, 7, color)
              val pov_point1 = you.coord + you.pov.rotateDeg(pov_angle) * pov_distance
              val pov_point2 = you.coord + you.pov.rotateDeg(-pov_angle) * pov_distance
              drawLine(you.coord, pov_point1, DARK_GRAY)
              drawLine(you.coord, pov_point2, DARK_GRAY)
              drawCircle(you.coord, human_audibility_radius, DARK_GRAY)
              drawCircle(you.coord, bullet_audibility_radius, DARK_GRAY)
            }
          })

          others.foreach {
            case player =>
              val player_color = if(player.team == you.team) ourPlayerColor(player, is_selected = false) else enemyPlayerColor(player)
              drawCircle(player.coord, 10, player_color)
              val info = if(player.team == yours.head.team) {
                s"${player.number_in_team+1}.${player.number+1} ${if(player.is_reloading) "перезарядка" else player.bullets}"
              } else {
                s"${player.number_in_team+1}.${player.number+1} ${if(player.is_reloading) "перезарядка" else player.bullets} ${(map.chanceToHit(you.coord,
                                                                                                                                                you.pov,
                                                                                                                                                you.isMoving,
                                                                                                                                                player.coord,
                                                                                                                                                player.isMoving)*100).toInt}%"
              }
              print(info, player.coord+number_place, max_font_size/globalScale, player_color, align = "center")
              val pov_point = player.coord + player.pov*100f
              drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), player_color)
              drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), player_color)
              val pov_point1 = player.coord + player.pov.rotateDeg(pov_angle) * pov_distance
              val pov_point2 = player.coord + player.pov.rotateDeg(-pov_angle) * pov_distance
              drawLine(player.coord, pov_point1, DARK_GRAY)
              drawLine(player.coord, pov_point2, DARK_GRAY)
              drawCircle(player.coord, human_audibility_radius, DARK_GRAY)
              drawCircle(player.coord, bullet_audibility_radius, DARK_GRAY)
          }

          your_bullets.foreach(b => {
            val color = if(b.player_number == selected_player) checkPausedColor(YELLOW) else checkPausedColor(GREEN)
            drawRectCentered(b.coord, bullet_size, bullet_size, color)
          })

          other_bullets.foreach(b => {
            val color = if(b.player_team == yours.head.team) checkPausedColor(GREEN) else checkPausedColor(RED)
            drawRectCentered(b.coord, bullet_size, bullet_size, color)
          })
        case None =>
      }
    }
  }

  interface {
    print(fps, 20, windowHeight-30, WHITE)
    if(!is_connected || !is_game_started) {
      print("Подключаемся к серверу...", windowCenter, DARK_GRAY, align = "center")
    } else {
      fireToggle match {
        case 0 => print("предохранитель", 20, 20, WHITE)
        case 1 => print("одиночный огонь", 20, 20, WHITE)
        case 2 => print("автоматический огонь", 20, 20, WHITE)
        case _ =>
      }
      /*current_state match {
        case Some(TacticServerData(yours, others, your_bullets, other_bullets, _)) =>
          val stats_builder = new StringBuilder
          val (your_wins, your_deaths) = yours.foldLeft((0, 0)) {case ((resw, resd), you) => (resw+you.wins, resd+you.deaths)}
          stats_builder append s"{$your_wins : $your_deaths} "
          val pewpew = others.groupBy(x => x.id).values.map(x => x.foldLeft((0, 0)) {case ((resw, resd), y) => (resw+y.wins, resd+y.deaths)})
          pewpew.zipWithIndex.foreach(x => stats_builder append s"${x._2+1} : {${x._1._1} : ${x._1._2}} ")
          print(stats_builder.toString().trim, 20, 20, checkPausedColor(WHITE))
        case None =>
      }*/
    }
    if(on_pause) {
      menu_items.zipWithIndex.foreach {
        case ((title, coord, _, color, _), idx) =>
          print(title, coord(), menuItemColor(idx, color), align = "center")
      }
    }
  }

  dispose {
    client.stop()
  }
}
