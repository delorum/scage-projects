package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import scala.collection.mutable
import scala.Some
import scala.collection.mutable.ArrayBuffer

class TutorialScreen extends ScageScreen("Tutorial Screen") {
  private val map = loadMap("map.ss")
  private val count = mutable.HashMap(1 -> 0, 2 -> 0)

  def gameStats:GameStats = {
    val teams_stats = count.toList.map {
      case (team, points) =>
        val player_stats = players.values.filter(_.team == team).map(p => {
          PlayerStats(p.team, p.number_in_team, p.number, p.wins, p.deaths)
        }).toList
        TeamStats(team, points, player_stats)
    }
    GameStats(teams_stats, game_start_moment_sec = None)
  }

  val player1 = TacticServerPlayer(
    id = 1,
    number = 0,
    team = 1,
    number_in_team = 0,
    map.respawnCoord(1),
    pov = Vec(0, 1),
    health = 100,
    wins = 0,
    deaths = 0)
  val player2 = player1.copy(number = 1, coord = map.respawnCoord(1))
  val player3 = player1.copy(number = 2, coord = map.respawnCoord(1))
  val player4 = TacticServerPlayer(
    id = 2,
    number = 3,
    team = 2,
    number_in_team = 0,
    map.respawnCoord(team = 2),
    pov = Vec(0, 1),
    health = 100,
    wins = 0,
    deaths = 0)
  val player5 = player4.copy(number = 4, coord = map.respawnCoord(team = 2))
  val player6 = player4.copy(number = 5, coord = map.respawnCoord(team = 2))

  private val players = Map(0 -> player1, 1 -> player2, 2 -> player3, 3 -> player4, 4 -> player5, 5 -> player6)

  private val bullets = ArrayBuffer[TacticServerBullet]()

  private var _center = Vec.zero
  private var selected_player = 0
  private var dir = Vec.zero
  private var pov_fixed = false

  private def fireToggle = {
    players(selected_player).fire_toggle
  }
  private def fireToggle_=(new_ft:Int) {
    players(selected_player).fire_toggle = new_ft
  }

  private val menu_items = createMenuItems(List(
    ("Продолжить",    () => Vec(windowWidth/2, windowHeight/2 + 30), WHITE, () => pauseOff()),
    ("Выход в меню",  () => Vec(windowWidth/2, windowHeight/2),      WHITE, () => stop()),
    ("Выход из игры", () => Vec(windowWidth/2, windowHeight/2-30),   WHITE, () => stopApp())
  ))

  private val number_place = Vec(1, 1).n*human_size*2

  private var selected_menu_item:Option[Int] = None
  private def menuItemColor(idx:Int, color:ScageColor):ScageColor = {
    selected_menu_item match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => color
    }
  }

  private val tutorial_texts = Array[String](
    "Обучение основам игры. Нажмите стрелку вправо",
    "Сообщения проматываются стрелками вправо/влево",
    "Перемещение поля зрения: WASD",
    "Приближение/удаление: колесико мышки",
    "Выйти из обучения в меню: Escape",
    "Под вашим началом отряд из трех бойцов",
    "Выбор бойца: 1, 2, 3. Нажатие еще раз: фокус на выбранном",
    "В этом обучении также доступны враги: 4, 5, 6",
    "Клик левой кнопкой: перемещение бойца",
    "Можно кликать несколько раз, прокладывая траекторию",
    "Клик правой кнопкой: зафиксировать направление взгляда бойца",
    "Левый шифт и левый контрол переключают режимы стрельбы",
    "Боец открывает огонь, если враг в поле зрения и оружие снято с предохранителя",
    ""
  )
  private var tutorial_position = 0

  private def selectPlayer(number:Int) {
    if(selected_player == number) {
      players.get(selected_player).map(_.coord).foreach(c => _center = c)
    } else {
      if(players(selected_player).team != players(number).team) {
        _center = players(number).coord
      }
      selected_player = number
    }
  }
  selectPlayer(selected_player)

  private def checkPausedColor(color:ScageColor):ScageColor = {
    if(on_pause) DARK_GRAY else color
  }

  private def ourPlayerColor(player:TacticServerPlayer, is_selected:Boolean):ScageColor = {
    if(on_pause) DARK_GRAY
    else if(player.isDead) GRAY
    else if(is_selected) YELLOW
    else GREEN
  }

  private def enemyPlayerColor(player:TacticServerPlayer):ScageColor = {
    if(on_pause) DARK_GRAY
    else if(player.isDead) GRAY
    else RED
  }

  key(KEY_1, onKeyDown = selectPlayer(0))
  key(KEY_2, onKeyDown = selectPlayer(1))
  key(KEY_3, onKeyDown = selectPlayer(2))
  key(KEY_4, onKeyDown = selectPlayer(3))
  key(KEY_5, onKeyDown = selectPlayer(4))
  key(KEY_6, onKeyDown = selectPlayer(5))

  key(KEY_SPACE, onKeyDown = players(selected_player).ds.clear())
  keyIgnorePause(KEY_ESCAPE, onKeyDown = switchPause())

  key(KEY_LSHIFT, onKeyDown = {
    if(fireToggle > 0) {
      fireToggle -= 1
    }
  })

  key(KEY_LCONTROL, onKeyDown = {
    if(fireToggle < 2) {
      fireToggle += 1
    }
  })

  key(KEY_W, 10, onKeyDown = dir += Vec(0, 1))
  key(KEY_A, 10, onKeyDown = dir += Vec(-1, 0))
  key(KEY_S, 10, onKeyDown = dir += Vec(0, -1))
  key(KEY_D, 10, onKeyDown = dir += Vec(1, 0))

  key(KEY_RIGHT, onKeyDown = {if(tutorial_position < tutorial_texts.length-1) tutorial_position += 1})
  key(KEY_LEFT, onKeyDown = {if(tutorial_position > 0) tutorial_position -= 1})

  leftMouseIgnorePause(
    onBtnDown = m => {
      if(!on_pause) {
        val sm = scaledCoord(m)
        if(isCoordInsideMapBorders(sm)) {
          players.get(selected_player).foreach(p => p.ds += sm)
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

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(!on_pause) {
      if(globalScale > 0.1f) {
        if(globalScale > 1) globalScale -= 1
        else globalScale -= 0.1f
      } else {
        // TODO: player stats scrolling
      }
    }
  })

  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(!on_pause) {
      if(globalScale < 1) globalScale += 0.1f
      else if(globalScale < 3) globalScale += 1
    } else {

    }
  })

  mouseMotion(onMotion = m => {
    if(!pov_fixed) {
      players.get(selected_player).foreach(p => p.pov = (scaledCoord(m) - p.coord).n)
    }
  })

  // update state
  action(10) {
    players.values.foreach(p => {
      p.ds.headOption.foreach(d => {
        if(d.dist2(p.coord) > human_speed*human_speed) {
          val new_coord = p.coord + (d - p.coord).n*human_speed
          if(map.isCoordCorrect(new_coord, human_size)) {
            p.coord = new_coord
            map.control_points.values.find(cp => (cp.team.isEmpty || cp.team.exists(cpt => cpt != p.team)) && coordOnArea(p.coord, cp.area)).foreach(cp => {
              cp.team = Some(p.team)
              cp.control_start_time_sec = System.currentTimeMillis()/1000
            })
          } else p.ds.clear()
        } else p.ds.remove(0)
      })
      if(p.canShoot && !map.isInsideSafeZone(p.coord)) {
        players
          .values
          .find(op =>
            op.team != p.team &&
            op.isAlive &&
            map.isCoordVisible(op.coord, p.coord, p.pov) &&
            !map.isInsideSafeZone(op.coord))
          .foreach(x => {
          val dir = (x.coord - p.coord).n
          bullets += p.shootBullet(dir)
        })
      }
      if(map.isInsideSafeZone(p.coord)) {
        if(p.isDead) p.health = 100
        if(p.bullets < max_bullets) p.replenishAmmo()
      }
    })
    bullets.foreach(b => {
      val new_coord = b.coord + b.dir*bullet_speed
      b.count -= 1
      if (!map.isPathCorrect(b.coord, new_coord, bullet_size)) {
        b.count = 0
      } else {
        b.prev_coord = b.coord
        b.coord = new_coord
        val damaged_players = players
          .values
          .filter(p =>
          p.team != b.shooter.team &&
            isBodyHit(b.prev_coord, b.coord, p.coord) &&
            !map.isInsideSafeZone(p.coord))
        if (damaged_players.nonEmpty) {
          damaged_players.foreach(p => {
            val chance = map.chanceToHit(b.shooter.coord, b.shooter.pov, b.shooter.isMoving, p.coord, p.isMoving)
            if(math.random < chance) {
              p.health -= bullet_damage
              if (p.health <= 0) {
                p.deaths += 1
                b.shooter.wins += 1
              }
            }
          })
          b.count = 0
        }
      }
    })
    bullets --= bullets.filter(b => b.count <= 0)
    map.control_points.values.foreach {
      case cp =>
        cp.team match {
          case Some(t) =>
            if(System.currentTimeMillis()/1000 - cp.control_start_time_sec > control_time_length_sec) {
              count(t) = count.getOrElse(t, 0) + 1
              cp.control_start_time_sec = System.currentTimeMillis()/1000
            }
          case None =>
        }
    }
    if(dir.notZero) {
      val new_center = _center + dir.n*human_size/2
      if(isCoordInsideMapBorders(new_center)) _center = new_center
      dir = Vec.zero
    }
  }

  center = _center

  render {
    // draw map
    val you = players(selected_player)

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
    map.control_points.foreach {
      case (number, cp @ ControlPoint(cp_number, team, control_start_time_sec, area)) =>
        val cp_color = cp.controlPointColor(you.team)
        drawSlidingLines(area, checkPausedColor(cp_color))
        team.foreach(t => {
          val time_left_sec = control_start_time_sec + control_time_length_sec - System.currentTimeMillis()/1000
          print(time_left_sec, cp.area_center, max_font_size/globalScale, cp_color, align = "center")
        })
    }
    val edges_color = checkPausedColor(GRAY)
    drawSlidingLines(map_edges, edges_color)

    val (your_players, enemy_players) = players.values.partition(_.team == you.team)
    your_players.foreach(you => {
      if(you.number == selected_player) {
        val color = ourPlayerColor(you, is_selected = true)
        drawCircle(you.coord, 10, color)
        print(s"${you.number_in_team+1}.${you.number+1} ${if(you.isReloading) "перезарядка" else you.bullets}", you.coord+number_place, max_font_size/globalScale, color)
        you.ds.foreach(d => drawFilledCircle(d, 3, color))
        if(you.ds.length > 0) {
          (you.coord :: you.ds.toList).sliding(2).foreach {
            case List(a, b) =>
              drawLine(a, b, color)
              print(f"${b.dist(a)/10f}%.2f m", a + (b - a).n * (b.dist(a) * 0.5f), max_font_size/globalScale, color)
          }
        }
        if(!pov_fixed && !on_pause) {
          val pov_point = you.coord + you.pov*100f
          drawLine(pov_point + Vec(5, -5), pov_point + Vec(-5, 5), color)
          drawLine(pov_point + Vec(-5, -5), pov_point + Vec(5, 5), color)
          if(pov_fixed) drawCircle(pov_point, 7, color)
          val pov_point1 = you.coord + you.pov.rotateDeg(pov_angle) * pov_distance
          val pov_point2 = you.coord + you.pov.rotateDeg(-pov_angle) * pov_distance
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

        val render_mouse = scaledCoord(mouseCoord)
        drawLine(you.coord, render_mouse, DARK_GRAY)
        val r = render_mouse.dist(you.coord)
        print(f"${r/10f}%.2f m", render_mouse, max_font_size/globalScale, DARK_GRAY)
      } else {
        val color = ourPlayerColor(you, is_selected = false)
        drawCircle(you.coord, 10, color)
        print(s"${you.number_in_team+1}.${you.number+1}  ${if(you.isReloading) "перезарядка" else you.bullets}", you.coord+number_place, max_font_size/globalScale, color)
        you.ds.foreach(d => drawFilledCircle(d, 3, color))
        if(you.ds.length > 0) {
          (you.coord :: you.ds.toList).sliding(2).foreach {
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

    enemy_players
      .filter(ep => {
        your_players.exists(y => map.isCoordVisibleOrAudible(ep.coord, y.coord, y.pov, is_moving = /*x.ds.nonEmpty*/true, human_audibility_radius))
      })
      .foreach {
        case player =>
          val player_color = if(player.team == you.team) ourPlayerColor(player, is_selected = false) else enemyPlayerColor(player)
          drawCircle(player.coord, 10, player_color)
          val info = if(player.team == your_players.head.team) {
            s"${player.number_in_team+1}.${player.number+1} ${if(player.isReloading) "перезарядка" else player.bullets}"
          } else {
            s"${player.number_in_team+1}.${player.number+1} ${if(player.isReloading) "перезарядка" else player.bullets} ${(map.chanceToHit(you.coord,
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

    val (your_bullets, other_bullets) = bullets.partition(_.shooter.team == you.team)

    your_bullets.foreach(b => {
      val color = if(b.shooter.number == selected_player) checkPausedColor(YELLOW) else checkPausedColor(GREEN)
      drawRectCentered(b.coord, bullet_size, bullet_size, color)
    })

    other_bullets
      .filter(ob => {
        your_players.exists(y => map.isCoordVisibleOrAudible(ob.coord, y.coord, y.pov, is_moving = true, bullet_audibility_radius))
      })
      .foreach(b => {
        val color = if(b.shooter.team == your_players.head.team) checkPausedColor(GREEN) else checkPausedColor(RED)
        drawRectCentered(b.coord, bullet_size, bullet_size, color)
      })
  }

  private def timeLeft(time_sec:Long):String = {
    val sec  = 1l
    val min  = sec*60
    s"${time_sec/min} мин ${time_sec%min/sec} сек"
  }
  interface {
    print(fps, windowWidth-20, windowHeight-10, WHITE, align = "top-right")
    if(!on_pause) {
      print(tutorial_texts(tutorial_position), 20, windowHeight-10, GREEN, align = "top-left")
      gameStats match {
        case GameStats(team_stats, _) =>
          /*val time_left = timeLeft(game_start_moment_sec + game_period_length_sec - System.currentTimeMillis()/1000)
          print(s"Осталось времени: $time_left", 20, windowHeight-10, WHITE, align = "top-left")*/
          print(s"Команда 1: ${team_stats.find(x => x.team == 1).map(_.team_points).getOrElse(0)} очков", 20, windowHeight-10-30, WHITE, align = "top-left")
          print(s"Команда 2: ${team_stats.find(x => x.team == 2).map(_.team_points).getOrElse(0)} очков", 20, windowHeight-10-30*2, WHITE, align = "top-left")
      }
    } else {
      gameStats match {
        case GameStats(team_stats, _) =>
          /*val time_left = timeLeft(game_start_moment_sec + game_period_length_sec - System.currentTimeMillis()/1000)
          print(s"Осталось времени: $time_left", 20, windowHeight-10, WHITE, align = "top-left")*/
          print(s"Команда 1: ${team_stats.find(x => x.team == 1).map(_.team_points).getOrElse(0)} очков", 20, windowHeight-10-30, WHITE, align = "top-left")
          val team1_player_stats = team_stats.find(x => x.team == 1).map(_.players_stats).getOrElse(Nil).sortBy(-_.wins)
          team1_player_stats.zipWithIndex.foreach {
            case (PlayerStats(team, number_in_team, number, wins, deaths), idx) =>
              val info = s"боец ${number_in_team+1}.${number+1} : убил: $wins умер: $deaths"
              print(info, 20, windowHeight-10-30*2-30*idx, WHITE, align = "top-left")
          }
          print(s"Команда 2: ${team_stats.find(x => x.team == 2).map(_.team_points).getOrElse(0)} очков", 20, windowHeight-10-30*3-30*team1_player_stats.length, WHITE, align = "top-left")
          val team2_player_stats = team_stats.find(x => x.team == 2).map(_.players_stats).getOrElse(Nil).sortBy(-_.wins)
          team2_player_stats.zipWithIndex.foreach {
            case (PlayerStats(team, number_in_team, number, wins, deaths), idx) =>
              val info = s"боец ${number_in_team+1}.${number+1} : убил: $wins умер: $deaths"
              print(info, 20, windowHeight-10-30*4-30*team1_player_stats.length-30*idx, WHITE, align = "top-left")
          }
      }
    }
    fireToggle match {
      case 0 => print("предохранитель", 20, 20, checkPausedColor(WHITE))
      case 1 => print("одиночный огонь", 20, 20, checkPausedColor(WHITE))
      case 2 => print("автоматический огонь", 20, 20, checkPausedColor(WHITE))
      case _ =>
    }
    if(on_pause) {
      menu_items.zipWithIndex.foreach {
        case ((title, coord, _, color, _), idx) =>
          print(title, coord(), menuItemColor(idx, color), align = "center")
      }
    }
  }
}
