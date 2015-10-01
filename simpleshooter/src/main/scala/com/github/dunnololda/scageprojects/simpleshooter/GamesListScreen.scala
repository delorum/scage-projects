package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet._
import play.api.libs.json._
import scala.collection.mutable.ArrayBuffer

class GamesListScreen extends ScageScreen("Games List Screen") {
  private val client = UdpNetClient(address = host, port = port, ping_timeout= 1000, check_if_offline_timeout = 5000)

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
  private val games_list = ArrayBuffer[GameInfo]()
  private val games_interface = ArrayBuffer[(InterfaceGameInfo, List[JoinOption])]()
  private var skip_games = 0

  private var is_connected = false

  private def buildGameListInterface() {
    games_interface.clear()
    games_interface ++= games_list.zipWithIndex.drop(skip_games).take(7).zipWithIndex.map {
            case ((gi @ GameInfo(game_id, team1_players, team2_players), game_number), idx) =>
              //println(s"$game_number $idx ${Vec(10, windowHeight-30-30*idx*2)}")
              val str1 = s"Игра ${game_number+1} : команда1: $team1_players игрок(ов), команда 2: $team2_players игрок(ов)"
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
  }

  /*private var selected_menu_item:Option[Int] = None
  private def menuItemColor(idx:Int, color:ScageColor):ScageColor = {
    selected_menu_item match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => color
    }
  }*/

  /*private val menu_items = createMenuItems(List(
    ("Создать", () => Vec(windowWidth/2, windowHeight/2 - 30), () => WHITE, () => {
      new TacticShooterClient(None).run()
      is_list_received = false
    }),
    ("Обновить", () => Vec(windowWidth/2, windowHeight/2 - 30*2), () => WHITE, () => is_list_received = false),
    ("Назад", () => Vec(windowWidth/2, windowHeight/2 - 30*3), () => WHITE, () => stop())
  ))*/

  key(KEY_F1, onKeyDown = {
    client.ignoreEvents = true
    new TacticShooterClient(None).run()
    client.ignoreEvents = false
    is_list_received = false
  })
  key(KEY_F5, onKeyDown = is_list_received = false)
  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(
    onBtnDown = m => {
      games_interface.flatMap(_._2).zipWithIndex.find {
        case (JoinOption(game_id, join_option, coord, area, option_team), idx) =>
          mouseOnArea(area)
      } match {
        case Some((JoinOption(game_id, join_option, coord, area, option_team), idx)) =>
          selected_game = Some(idx)
        case None =>
          /*menu_items.zipWithIndex.find(x => mouseOnArea(x._1._3())) match {
            case Some((pewpew, idx)) => selected_menu_item = Some(idx)
            case None =>
          }*/
      }
    },
    onBtnUp = m => {
      selected_game match {
        case Some(idx) =>
          val all_join_options = games_interface.flatMap(_._2)
          if(idx >= 0 && all_join_options.length > idx) {
            val JoinOption(game_id, _, _, _, option_team) = all_join_options(idx)
            client.ignoreEvents = true
            new TacticShooterClient(Some(JoinGame(game_id, option_team))).run()
            client.ignoreEvents = false
            selected_game = None
            is_list_received = false
          }
        case None =>
          /*selected_menu_item match {
            case Some(idx) =>
              menu_items(idx)._5()
              selected_menu_item = None
            case None =>
          }*/
      }
    }
  )

  mouseWheelDown(onWheelDown = m => {
    if(skip_games+7 < games_list.length) skip_games += 1
    buildGameListInterface()
  })

  mouseWheelUp(onWheelUp = m => {
    if(skip_games > 0) {
      skip_games -= 1
      buildGameListInterface()
    }
  })

  implicit val VecJson_reader = {
    case class VecJson(x:Float, y:Float)
    import play.api.libs.functional.syntax._
    (
      (__ \ "x").read[Float] and
      (__ \ "y").read[Float]
    )(VecJson.apply _).map(z => Vec(z.x, z.y))
  }
  implicit val ControlPointData_reader = Json.reads[ControlPointData]
  implicit val GameInfoData_reader = Json.reads[GameInfoData]
  implicit val Wall_reader = Json.reads[Wall]
  implicit val GameMapData_reader = Json.reads[GameMapData]
  implicit val TacticServerPlayerData_reader = Json.reads[TacticServerPlayerData]
  implicit val TacticServerBulletData_reader = Json.reads[TacticServerBulletData]
  implicit val PlayerStatsData_reader = Json.reads[PlayerStatsData]
  implicit val TeamStatsData_reader = Json.reads[TeamStatsData]
  implicit val GameStatsData_reader = Json.reads[GameStatsData]
  implicit val TacticShooterServerData_reader = Json.reads[TacticShooterServerData]

  // receive data
  actionStaticPeriod(10) {
    client.newEvent {
      case NewUdpServerData(received_data) =>
        received_data.validate[TacticShooterServerData] match {
          case JsSuccess(data, _ ) =>
            if(data.gameslist.nonEmpty) {
              games_list.clear()
              games_list ++= gamesList(data)
              buildGameListInterface()
              is_list_received = true
            }
          case JsError(error) =>
            println(s"[client] failed to parse server data $received_data: $error")
        }
      case UdpServerConnected => is_connected = true
      case UdpServerDisconnected => is_connected = false
    }
  }

  // send data
  actionStaticPeriod(1000) {
    if(!is_list_received) {
      client.send(Json.obj("gameslist" -> true))
    }
  }

  interface {
    if(!is_connected) {
      print("Подключаемся к серверу...", windowCenter, DARK_GRAY, align = "center")
    } else {
      if(!is_list_received) {
        print("Получаем список игр...", windowCenter, DARK_GRAY, align = "center")
      } else {
        if(games_interface.length == 0) {
          print("Игры не найдены", windowCenter, WHITE, align = "center")
        } else {
          games_interface.map(_._1).foreach {
            case InterfaceGameInfo(message, coord) =>
              print(message, coord, WHITE)
          }
          games_interface.flatMap(_._2).zipWithIndex.foreach {
            case (JoinOption(_, message, coord, area, _), idx) =>
              print(message, coord, gameColor(idx))
          }
        }
        /*menu_items.zipWithIndex.foreach {
          case ((title, coord, _, color, _), idx) =>
            print(title, coord(), menuItemColor(idx, color()), align = "center")
        }*/
      }
    }
    print("Создать: F1, Обновить: F5, Выход в меню: Escape, Прокрутка: Колесико", 10, 10, GREEN)
  }

  dispose {
    client.stop()
    Unit
  }
}
