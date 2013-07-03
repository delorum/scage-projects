package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, UdpServerDisconnected, UdpServerConnected, NewUdpServerData, UdpNetClient}
import scala.collection.mutable.ArrayBuffer

class GamesListScreen extends ScageScreen("Games List Screen") {
  private val client = UdpNetClient(address = host, port = port, ping_timeout= 1000, check_timeout = 5000)

  private var is_list_received = false
  private var selected_game:Option[Int] = None
  private def gameColor(idx:Int):ScageColor = {
    selected_game match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => WHITE
    }
  }

  private def selectGame(game_id:Int) {
    if(game_id >= 0 && games_list.length > game_id) selected_game = Some(game_id)
  }
  private def runSelectedGame() {
    selected_game match {
      case Some(gid) =>
        if(gid >= 0 && games_list.length > gid) {
          new TacticShooterClient(Some(games_list(gid)._1.game_id)).run()
          selected_game = None
        }
      case None =>
    }
  }

  private val games_list = ArrayBuffer[(GameInfo, String, Vec, List[Vec])]()
  private var is_connected = false

  private var selected_menu_item:Option[Int] = None
  private def menuItemColor(idx:Int, color:ScageColor):ScageColor = {
    selected_menu_item match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => color
    }
  }

  private val menu_items = createMenuItems(List(
    ("Создать", () => Vec(windowWidth/2, windowHeight/2 - 30), () => WHITE, () => new TacticShooterClient(None).run()),
    ("Назад", () => Vec(windowWidth/2, windowHeight/2 - 30*2), () => WHITE, () => stop())
  ))

  key(KEY_1, onKeyDown = selectGame(0), onKeyUp = runSelectedGame())
  key(KEY_2, onKeyDown = selectGame(1), onKeyUp = runSelectedGame())
  key(KEY_3, onKeyDown = selectGame(2), onKeyUp = runSelectedGame())
  key(KEY_4, onKeyDown = selectGame(3), onKeyUp = runSelectedGame())
  key(KEY_5, onKeyDown = selectGame(4), onKeyUp = runSelectedGame())
  key(KEY_6, onKeyDown = selectGame(5), onKeyUp = runSelectedGame())
  key(KEY_7, onKeyDown = selectGame(6), onKeyUp = runSelectedGame())
  key(KEY_8, onKeyDown = selectGame(7), onKeyUp = runSelectedGame())
  key(KEY_9, onKeyDown = selectGame(8), onKeyUp = runSelectedGame())
  key(KEY_F5, onKeyDown = is_list_received = false)
  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(
    onBtnDown = m => {
      games_list.zipWithIndex.find(x => mouseOnArea(x._1._4)) match {
        case Some((game, idx)) =>
          selected_game = Some(idx)
        case None =>
          menu_items.zipWithIndex.find(x => mouseOnArea(x._1._3())) match {
            case Some((pewpew, idx)) => selected_menu_item = Some(idx)
            case None =>
          }
      }
    },
    onBtnUp = m => {
      selected_game match {
        case Some(gid) =>
          if(gid >= 0 && games_list.length > gid) {
            new TacticShooterClient(Some(games_list(gid)._1.game_id)).run()
            selected_game = None
          }
        case None =>
          selected_menu_item match {
            case Some(idx) =>
              menu_items(idx)._5()
              selected_menu_item = None
            case None =>
          }
      }
    }
  )

  // receive data
  action(10) {
    client.newEvent {
      case NewUdpServerData(message) =>
        if(message.contains("gameslist")) {
          val new_games_list = gamesList(message).zipWithIndex.map {
            case (gi, idx) =>
              val str = s"Игра ${idx+1} : ${gi.players} игрок(ов)"
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
        } else {
          games_list.zipWithIndex.foreach {
            case ((GameInfo(game_id, players), str, coord, area), idx) =>
              print(str, coord, gameColor(idx))
          }
        }
        menu_items.zipWithIndex.foreach {
          case ((title, coord, _, color, _), idx) =>
            print(title, coord(), menuItemColor(idx, color()), align = "center")
        }
      }
    }
    print("Обновить список: F5, Выход в меню: Escape", 10, 10, GREEN)
  }

  dispose {
    client.stop()
  }
}
