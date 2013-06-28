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
  private val map = loadMap(map_name)

  /*private val players = mutable.HashMap[Long, List[TacticServerPlayer]]()
  private val bullets = ArrayBuffer[TacticBullet]()*/

  private val games = mutable.HashMap[Int, TacticGame]()  // game_id -> game

  private val games_by_clientid = mutable.HashMap[Long, TacticGame]() // client_id -> game

  private val numbers_in_teams = mutable.HashMap[Int, ArrayBuffer[Boolean]]() // team -> list of positions

  private val client_builders = mutable.HashMap[Long, StateBuilder]()
  private def clientBuilder(client_id:Long) = client_builders.getOrElseUpdate(client_id, NetState.newBuilder)

  private def addNewPlayerToGame(client_id:Long, game:TacticGame) {
    val team = {
      val team1_amount = game.players.values.flatten.count(_.team == 1)
      val team2_amount = game.players.size - team1_amount
      if(team1_amount <= team2_amount) 1
      else 2
    }

    val number_in_team = {
      val maybe_position = numbers_in_teams.getOrElseUpdate(team, ArrayBuffer[Boolean]()).indexWhere(x => !x)
      if(maybe_position == -1) {
        numbers_in_teams(team) += true
        numbers_in_teams(team).length-1
      } else maybe_position
    }

    val coord1 = respawnCoord(team, map_width, map_height, body_radius, map)
    val coord2 = respawnCoord(team, map_width, map_height, body_radius, map)
    val coord3 = respawnCoord(team, map_width, map_height, body_radius, map)

    val player1 = TacticServerPlayer(
      client_id,
      0,
      team,
      number_in_team,
      coord1,
      pov = Vec(0, 1),
      health = 100,
      wins = 0,
      deaths = 0,
      visible = true)
    val player2 = player1.copy(number = 1, coord = coord2)
    val player3 = player1.copy(number = 2, coord = coord3)
    game.players(client_id) = List(player1, player2, player3)
    games_by_clientid += (client_id -> game)
  }

  // receive data
  action(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
      case NewUdpClientData(client_id, message) =>
        //println(message.toJsonString)
        val builder = clientBuilder(client_id)
        if(message.contains("gameslist")) {
          builder += ("gameslist" -> games.map(x => GameInfo(x._1, x._2.players.size).netState).toList)
        } else if(message.contains("create")) {
          games_by_clientid.get(client_id) match {
            case Some(game) =>
              builder += ("gamestarted" -> true, "map" -> map.netState)
            case None =>
              val new_game_id = nextId
              val new_game = new TacticGame(new_game_id)
              games += (new_game_id -> new_game)
              addNewPlayerToGame(client_id, new_game)
              builder += ("gamestarted" -> true, "map" -> map.netState)
          }
        } else if(message.contains("join")) {
          games_by_clientid.get(client_id) match {
            case Some(game) =>
              builder += ("gamestarted" -> true, "map" -> map.netState)
            case None =>
              games.get(message.value[Int]("join").get) match {
                case Some(game) =>
                  addNewPlayerToGame(client_id, game)
                  builder += ("gamestarted" -> true, "map" -> map.netState)
                case None =>
                  builder += ("gamenotfound" -> true)
              }
          }
        } else {
          games_by_clientid.get(client_id) match {
            case Some(TacticGame(_, players, bullets)) =>
              val client_players = players(client_id)
              if(message.contains("sendmap")) builder += ("map" -> map.netState)
              val TacticClientData(player_num, destination, pov, fire_toggle, clear_destinations) = tacticClientData(message)
              val player = client_players(player_num)
              if(clear_destinations) player.ds.clear()
              destination.foreach(d => {
                player.ds += d
                builder += ("dests_cleared" -> true)
              })
              pov.foreach(p => player.pov = (p - player.coord).n)
              fire_toggle.foreach(ft => {
                player.fire_toggle = ft
                builder += ("fire_toggle_set" -> true)
              })
            case None =>
          }
        }
      case UdpClientDisconnected(client_id) =>
        games_by_clientid.get(client_id) match {
          case Some(TacticGame(game_id, players, bullets)) =>
            games_by_clientid -= client_id
            for {
              client_players <- players.get(client_id)
              head_player <- client_players.headOption
              team_numbers <- numbers_in_teams.get(head_player.team)
              if head_player.number_in_team >= 0 && head_player.number_in_team < team_numbers.length
            } {
              team_numbers(head_player.number_in_team) = false
            }
            players.get(client_id).foreach(x => {
              x.headOption.foreach(h => {

              })
              x.head.number_in_team
            })
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
              val new_coord = p.coord + (d - p.coord).n*speed
              if(isCoordCorrect(new_coord, body_radius, map.walls)) {
                p.coord = new_coord
              } else p.ds.clear()
            } else p.ds.remove(0)
          })
          if(p.canShoot && !map.isInsideSafeZone(p.coord)) {
            players
              .values
              .flatten
              .find(op => op.team != p.team && op.isAlive && isCoordVisible(op.coord, p.coord, p.pov, pov_distance, pov_angle, map.walls) && !map.isInsideSafeZone(op.coord))
              .foreach(x => {
              val dir = (x.coord - p.coord).n
              bullets += p.shootBullet(nextId, dir, body_radius, bullet_count)
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
          if (!isPathCorrect(b.coord, new_coord, bullet_size, map.walls)) {
            b.count = 0
          } else {
            b.prev_coord = b.coord
            b.coord = new_coord
            val damaged_players = players
              .values
              .flatten
              .filter(p =>
                p.team != b.shooter.team &&
                (isBodyHit(b.prev_coord, b.coord, p.coord, body_radius) ||
                p.coord.dist2(b.coord) < body_radius*body_radius) &&
                !map.isInsideSafeZone(p.coord))
            if (damaged_players.nonEmpty) {
              damaged_players.foreach(p => {
                val chance = chanceToHit(b.shooter.coord, b.shooter.pov, pov_distance, pov_angle, b.shooter.isMoving, p.coord, p.isMoving, map)
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
    }
  }

  // send data
  action(50) {
    games.values.foreach {
      case TacticGame(_, players, bullets) =>
        players.foreach {
          case (id, client) =>
            val builder = clientBuilder(id)
            val (your_players, other_players) = players.values.flatten.partition(_.id == id)
            val yours = your_players.map(x => x.netState).toList
            builder += ("yours" -> yours)
            val others = other_players.view
                                      .map(x => x.copy(visible = your_players.exists(y => isCoordVisibleOrAudible(x.coord,
                                                                                                                  y.coord,
                                                                                                                  y.pov, pov_distance, pov_angle,
                                                                                                                  is_moving = /*x.ds.nonEmpty*/true,
                                                                                                                  human_audibility_radius,
                                                                                                                  map.walls))))
                                      .filter(_.visible)
                                      .map(_.netState).toList
            if(others.nonEmpty) builder += ("others" -> others)
            val (your_bullets, other_bullets) = bullets.filter(b => your_players.exists(y => isCoordVisibleOrAudible(b.coord,
                                                                                                                     y.coord,
                                                                                                                     y.pov,
                                                                                                                     pov_distance,
                                                                                                                     pov_angle,
                                                                                                                     is_moving = true,
                                                                                                                     bullet_audibility_radius, map.walls))).partition(_.shooter.id == id)
            if (your_bullets.nonEmpty) builder += ("your_bullets" -> your_bullets.map(_.netState).toList)
            if (other_bullets.nonEmpty) builder += ("other_bullets" -> other_bullets.map(_.netState).toList)
            val data = NetState(builder.toState)
            builder.clear()
            server.sendToClient(id, data)
        }
    }
    client_builders.filter(_._2.nonEmpty).foreach(x => {
      server.sendToClient(x._1, x._2.toState)
      x._2.clear()
    })
  }
}
