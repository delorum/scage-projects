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

  case class InterfaceGameInfo(message:String, coord:Vec)
  case class JoinOption(game_id:Int, message:String, coord:Vec, area:List[Vec], option_team:Option[Int])
  private val games_list = ArrayBuffer[(InterfaceGameInfo, List[JoinOption])]()
  private var is_connected = false

  private var selected_menu_item:Option[Int] = None
  private def menuItemColor(idx:Int, color:ScageColor):ScageColor = {
    selected_menu_item match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => color
    }
  }

  private val menu_items = createMenuItems(List(
    ("Создать", () => Vec(windowWidth/2, windowHeight/2 - 30), () => WHITE, () => {
      new TacticShooterClient(None).run()
      is_list_received = false
    }),
    ("Назад", () => Vec(windowWidth/2, windowHeight/2 - 30*2), () => WHITE, () => stop())
  ))

  key(KEY_F5, onKeyDown = is_list_received = false)
  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(
    onBtnDown = m => {
      games_list.flatMap(_._2).zipWithIndex.find {
        case (JoinOption(game_id, join_option, coord, area, option_team), idx) =>
          mouseOnArea(area)
      } match {
        case Some((JoinOption(game_id, join_option, coord, area, option_team), idx)) =>
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
        case Some(idx) =>
          val all_join_options = games_list.flatMap(_._2)
          if(idx >= 0 && all_join_options.length > idx) {
            val JoinOption(game_id, _, _, _, option_team) = all_join_options(idx)
            new TacticShooterClient(Some(JoinGame(game_id, option_team))).run()
            selected_game = None
            is_list_received = false
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
          val new_games_list:List[(InterfaceGameInfo, List[JoinOption])] = gamesList(message).zipWithIndex.map {
            case (gi @ GameInfo(game_id, team1_players, team2_players), idx) =>
              val str1 = s"Игра ${idx+1} : команда1: $team1_players, команда 2: $team2_players"
              val coord1 = Vec(10, windowHeight-30-30*idx*2)
              val igi = InterfaceGameInfo(str1, coord1)

              val str2 = "За любую команду"
              val coord2 = Vec(10, windowHeight-30*2-30*idx*2)
              val area2 = messageArea(str2, coord2)

              val str3 = "За Команду 1"
              val coord3 = Vec(10+200, windowHeight-30*2-30*idx*2)
              val area3 = messageArea(str3, coord3)

              val str4 = "За команду 2"
              val coord4 = Vec(10+200*2, windowHeight-30*2-30*idx*2)
              val area4 = messageArea(str4, coord4)

              (igi, List(
                JoinOption(game_id, str2, coord2, area2, None),
                JoinOption(game_id, str3, coord3, area3, Some(1)),
                JoinOption(game_id, str4, coord4, area4, Some(2))
              ))
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
          games_list.map(_._1).foreach {
            case InterfaceGameInfo(message, coord) =>
              print(message, coord, WHITE)
          }
          games_list.flatMap(_._2).zipWithIndex.foreach {
            case (JoinOption(_, message, coord, area, _), idx) =>
              print(message, coord, gameColor(idx))
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
