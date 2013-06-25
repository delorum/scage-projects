package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, UdpServerDisconnected, UdpServerConnected, NewUdpServerData, UdpNetClient}
import scala.collection.mutable.ArrayBuffer

class GamesListScreen extends ScageScreen("Games List Screen") {
  private val client = UdpNetClient(address = host, port = port, ping_timeout= 1000, check_timeout = 5000)

  private var is_list_received = false
  private val games_list = ArrayBuffer[(GameInfo, String, Vec, List[Vec])]()
  private var is_connected = false

  private val menu_items:List[(String, Vec, List[Vec], () => Any)] = createMenuItems(List(
    ("Создать", Vec(windowWidth/2, windowHeight/2 - 30), () => new TacticShooterClient(None).run())
  ))

  key(KEY_1, onKeyDown = if(games_list.length > 0) new TacticShooterClient(Some(games_list(0)._1.game_id)).run())
  key(KEY_2, onKeyDown = if(games_list.length > 1) new TacticShooterClient(Some(games_list(1)._1.game_id)).run())
  key(KEY_3, onKeyDown = if(games_list.length > 2) new TacticShooterClient(Some(games_list(2)._1.game_id)).run())
  key(KEY_4, onKeyDown = if(games_list.length > 3) new TacticShooterClient(Some(games_list(3)._1.game_id)).run())
  key(KEY_5, onKeyDown = if(games_list.length > 4) new TacticShooterClient(Some(games_list(4)._1.game_id)).run())
  key(KEY_6, onKeyDown = if(games_list.length > 5) new TacticShooterClient(Some(games_list(5)._1.game_id)).run())
  key(KEY_7, onKeyDown = if(games_list.length > 6) new TacticShooterClient(Some(games_list(6)._1.game_id)).run())
  key(KEY_8, onKeyDown = if(games_list.length > 7) new TacticShooterClient(Some(games_list(7)._1.game_id)).run())
  key(KEY_9, onKeyDown = if(games_list.length > 8) new TacticShooterClient(Some(games_list(8)._1.game_id)).run())
  key(KEY_F5, onKeyDown = is_list_received = false)
  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(onBtnDown = m => {
    games_list.find {
      case (GameInfo(game_id, players), str, coord, area) =>
        mouseOnArea(area)
    } match {
      case Some((GameInfo(game_id, players), str, coord, area)) =>
        new TacticShooterClient(Some(game_id)).run()
      case None =>
        menu_items.find(x => mouseOnArea(x._3)) match {
          case Some((_, _, _, action)) => action()
          case None =>
        }
    }
  })

  // receive data
  action(10) {
    client.newEvent {
      case NewUdpServerData(message) =>
        if(message.contains("gameslist")) {
          val new_games_list = gamesList(message).zipWithIndex.map {
            case (gi, idx) =>
              val str = s"Game ${idx+1} : ${gi.players} player(s)"
              val coord = Vec(10, windowHeight-30-30*idx)
              val area = messageArea(str, coord)
              (gi, str, coord, area)
          }
          games_list.clear()
          games_list ++= new_games_list
          is_list_received = true
        }
      case UdpServerConnected => is_connected = true
      case UdpServerDisconnected => is_connected = false
    }
  }

  // send data
  action(1000) {
    if(!is_list_received) {
      client.send(NetState("gameslist" -> true))
    }
  }

  interface {
    if(!is_connected) {
      print("Подключаемся к серверу...", windowCenter, DARK_GRAY, align = "center")
    } else {
      if(!is_list_received) {
        print("Получаем список игр...", windowCenter, DARK_GRAY, align = "center")
      } else {
        if(games_list.length == 0) {
          print("Игры не найдены", windowCenter, WHITE, align = "center")
          menu_items.foreach {
            case (title, coord, _, _) =>
              print(title, coord, WHITE, align = "center")
          }
        } else {
          games_list.foreach {
            case (GameInfo(game_id, players), str, coord, area) =>
              print(str, coord, WHITE)
          }
        }
      }
    }
    print("Обновить список: F5, Выход в меню: Escape", 10, 10, GREEN)
  }

  dispose {
    client.stop()
  }
}
