package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.cli.Cli
import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, _}
import collection.mutable
import com.github.dunnololda.simplenet.NewUdpClientData
import com.github.dunnololda.simplenet.NewUdpConnection
import collection.mutable.ArrayBuffer

object TacticShooterServer extends ScageApp("TacticShooter") with Cli {
  programDescription = s"Tactic Shooter Server v$appVersion"
  commandLineArgsAndParse(
    ("p", "port", s"port to bind server on. default: $port", true, false),
    ("m", "map",  "map file to load. default: map.ss", true, false)
  )
  private val server = UdpNetServer(port = property("port", port), ping_timeout= 1000, check_timeout = 5000)

  private val map_name = property("map", "map.ss")
  private val walls = loadMap(map_name)

  /*private val players = mutable.HashMap[Long, List[TacticServerPlayer]]()
  private val bullets = ArrayBuffer[TacticBullet]()*/

  private val games = mutable.HashMap[Int, TacticGame]()

  private val games_by_clientid = mutable.HashMap[Long, TacticGame]()

  private def addNewPlayerToGame(client_id:Long, game:TacticGame) {
    val coord1 = randomCoord(map_width, map_height, None, body_radius, walls)
    val coord2 = randomCoord(map_width, map_height, Some(coord1), body_radius, walls)
    val coord3 = randomCoord(map_width, map_height, Some(coord1), body_radius, walls)
    val player1 = TacticServerPlayer(
      client_id,
      0,
      coord1,
      _pov = Vec(0, 1),
      health = 100,
      wins = 0,
      deaths = 0,
      visible = true)
    val player2 = player1.copy(number = 1, _coord = coord2)
    val player3 = player1.copy(number = 2, _coord = coord3)
    game.players(client_id) = List(player1, player2, player3)
    games_by_clientid += (client_id -> game)
  }

  // receive data
  action(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
      case NewUdpClientData(client_id, message) =>
        if(message.contains("gameslist")) {
          server.sendToClient(client_id, NetState("gameslist" -> games.map(x => GameInfo(x._1, x._2.players.size).netState).toList))
        } else if(message.contains("create")) {
          if(!games_by_clientid.contains(client_id)) {
            val new_game_id = nextId
            val new_game = new TacticGame(new_game_id)
            games += (new_game_id -> new_game)
            addNewPlayerToGame(client_id, new_game)
            server.sendToClient(client_id, NetState("gamestarted" -> true, "walls" -> walls.map(_.netState).toList))
          }
        } else if(message.contains("join")) {
          if(!games_by_clientid.contains(client_id)) {
            games.get(message.value[Int]("join").get) match {
              case Some(game) =>
                addNewPlayerToGame(client_id, game)
                server.sendToClient(client_id, NetState("gamestarted" -> true, "walls" -> walls.map(_.netState).toList))
              case None =>
                server.sendToClient(client_id, NetState("gamenotfound" -> true))
                server.disconnectClient(client_id)
            }
          }
        } else {
          games_by_clientid.get(client_id) match {
            case Some(TacticGame(_, players, bullets)) =>
              val client_players = players(client_id)
              if(message.contains("sendmap")) server.sendToClient(client_id, NetState("walls" -> walls.map(_.netState).toList))
              val TacticClientData(player_num, destination, pov, clear_destinations) = tacticClientData(message)
              val player = client_players(player_num)
              if(clear_destinations) player.ds.clear()
              destination.foreach(d => player.ds += d)
              pov.foreach(p => player.pov = (p - player.coord).n)
            case None =>
          }
        }
      case UdpClientDisconnected(client_id) =>
        games_by_clientid.get(client_id) match {
          case Some(TacticGame(game_id, players, bullets)) =>
            games_by_clientid -= client_id
            players -= client_id
            if(players.size == 0) {
              games -= game_id
            }
          case None =>
        }
    }
  }

  // update state
  action(10) {
    games.values.foreach {
      case TacticGame(_, players, bullets) =>
        players.values.flatten.foreach(p => {
          p.ds.headOption.foreach(d => {
            if(d.dist2(p.coord) > speed*speed) {
              val new_coord = outsideCoord(p.coord + (d - p.coord).n*speed, map_width, map_height)
              if(isCoordCorrect(new_coord, body_radius, walls)) {
                p.coord = new_coord
              } else p.ds.clear()
            } else p.ds.remove(0)
          })
          if(System.currentTimeMillis() - p.last_bullet_shot > fire_pace) {
            players
              .values
              .flatten
              .find(op => op.id != p.id && isCoordVisible(op.coord, p.coord, p.povArea, walls))
              .foreach(x => {
              val dir = (x.coord - p.coord).n
              val init_coord = p.coord + dir*(body_radius+1)
              bullets += TacticBullet(nextId, dir, p, init_coord, init_coord, bullet_count)
              p.last_bullet_shot = System.currentTimeMillis()
            })
          }
        })
        bullets.foreach(b => {
          val new_coord = b.coord + b.dir*bullet_speed
          b.count -= 1
          if (!isPathCorrect(b.coord, new_coord, bullet_size, walls)) {
            b.count = 0
          } else {
            b.prev_coord = b.coord
            b.coord = new_coord
            val damaged_players = players
              .values
              .flatten
              .filter(p => isBodyHit(b.prev_coord, b.coord, p.coord, body_radius) ||
              p.coord.dist2(b.coord) < body_radius*body_radius)
            if (damaged_players.nonEmpty) {
              damaged_players.foreach(p => {
                p.health -= bullet_damage
                if (p.health <= 0) {
                  p.deaths += 1
                  p.ds.clear()
                  p.coord = randomCoord(map_width, map_height, None, body_radius, walls)
                  p.pov = Vec(0, 1)
                  p.health = 100
                  b.shooter.wins += 1
                }
              })
              b.count = 0
            }
          }
        })
        bullets --= bullets.filter(b => b.count <= 0)
    }
  }

  // send data
  action(50) {
    games.values.foreach {
      case TacticGame(_, players, bullets) =>
        players.foreach {
          case (id, client) =>
            val builder = ArrayBuffer[(String, Any)]()
            val (your_players, other_players) = players.values.flatten.partition(_.id == id)
            val yours = your_players.map(x => x.netState).toList
            builder += ("yours" -> yours)
            val others = other_players.map(x => x.copy(visible = your_players.exists(y => isCoordVisibleOrAudible(x.coord, y.coord, y.povArea, is_moving = /*x.ds.nonEmpty*/true, audibility_radius, walls))).netState).toList
            if(others.nonEmpty) builder += ("others" -> others)
            val (your_bullets, other_bullets) = bullets.filter(b => your_players.exists(y => isCoordVisibleOrAudible(b.coord, y.coord, y.povArea, is_moving = true, audibility_radius, walls))).partition(_.shooter.id == id)
            if (your_bullets.nonEmpty) builder += ("your_bullets" -> your_bullets.map(_.netState).toList)
            if (other_bullets.nonEmpty) builder += ("other_bullets" -> other_bullets.map(_.netState).toList)
            val data = NetState(builder:_*)
            server.sendToClient(id, data)
        }
    }
  }
}
