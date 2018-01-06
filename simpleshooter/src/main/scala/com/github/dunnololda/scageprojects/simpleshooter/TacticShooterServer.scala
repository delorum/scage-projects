package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.cli.Cli
import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{NewUdpClientData, NewUdpConnection, _}
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



object TacticShooterServer extends ScageApp("TacticShooter") with Cli {
  programDescription = s"Tactic Shooter Server v$appVersion"
  commandLineArgsAndParse(
    ("p", "port", s"port to bind server on. default: $port", true, false),
    ("m", "map",  "map file to load. default: map.ss", true, false)
  )
  private val server = UdpNetServer(port = property("port", port), ping_timeout= 1000, check_if_offline_timeout = 5000)

  private val map_name = property("map", "map.ss")
  private val preloaded_map = loadMap(map_name) // TODO: allow to select map on creation
  private val path_finder = new GameMapPathFinder(preloaded_map)

  private val games = mutable.HashMap[Int, TacticGame]()  // game_id -> game

  private val games_by_clientid = mutable.HashMap[Long, TacticGame]() // client_id -> game

  private val numbers_in_teams = mutable.HashMap[Int, ArrayBuffer[Boolean]]() // team -> list of positions

  private val client_builders = mutable.HashMap[Long, (ArrayBuffer[(String, JsValue)], TacticClientStuff)]()
  private def nonEmptyClientBuilders = client_builders.withFilter(x => x._2._1.nonEmpty).map(x => (x._1, x._2._1))
  private def stuffForClients(client_ids:Seq[Long]) = client_builders.withFilter(x => client_ids.contains(x._1)).map(_._2._2)
  private def clientBuilderAndStuff(client_id:Long) = client_builders.getOrElseUpdate(client_id, (ArrayBuffer[(String, JsValue)](), TacticClientStuff()))

  private def addNewPlayerToGame(client_id:Long, game:TacticGame, wanted_team:Option[Int]) {
    val team = wanted_team.filter(x => x == 1 || x == 2).getOrElse({
      val all_players =  game.players.values.flatten.toSeq
      val team1_amount = all_players.count(_.team == 1)
      val team2_amount = all_players.length - team1_amount
      if(team1_amount <= team2_amount) 1
      else 2
    })

    //println(numbers_in_teams)
    val number_in_team = {
      val maybe_position = numbers_in_teams.getOrElseUpdate(team, ArrayBuffer[Boolean]()).indexWhere(x => !x)
      if(maybe_position == -1) {
        numbers_in_teams(team) += true
        numbers_in_teams(team).length-1
      } else {
        numbers_in_teams(team)(maybe_position) = true
        maybe_position
      }
    }
    //println(number_in_team)

    val coord1 = game.map.respawnCoord(team)
    val coord2 = game.map.respawnCoord(team)
    val coord3 = game.map.respawnCoord(team)

    val player1 = TacticServerPlayer(
      client_id,
      0,
      team,
      number_in_team,
      coord1,
      pov = Vec(0, 1),
      health = 100,
      wins = 0,
      deaths = 0)
    val player2 = player1.copy(number = 1, coord = coord2)
    val player3 = player1.copy(number = 2, coord = coord3)
    game.players(client_id) = List(player1, player2, player3)
    games_by_clientid += (client_id -> game)
    stuffForClients(game.players.keys.toSeq).foreach(_.game_stats_update_required = true)
  }

  implicit val VecJson_reader = {
    case class VecJson(x:Float, y:Float)
    import play.api.libs.functional.syntax._
    (
      (__ \ "x").read[Float] and
      (__ \ "y").read[Float]
    )(VecJson.apply _).map(z => Vec(z.x, z.y))
  }
  implicit val JoinGameData_reader = Json.reads[JoinGameData]
  implicit val TacticShooterClientData_reader = Json.reads[TacticShooterClientData]

  // receive data
  actionStaticPeriod(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
      case NewUdpClientData(client_id, received_data) =>
        received_data.validate[TacticShooterClientData] match {
          case JsSuccess(data, _) =>
            val (fields, client_stuff) = clientBuilderAndStuff(client_id)
            if(data.gameslist.exists(x => x)) {
              val games_list = games.filter(!_._2.isFinished).map(x => {
                val all_players = x._2.players.values.flatten
                GameInfo(x._1, all_players.count(_.team == 1)/3, all_players.count(_.team == 2)/3).netState
              }).toList
              fields += ("gameslist" -> JsArray(games_list))
            } else if(data.create.exists(x => x)) {
              games_by_clientid.get(client_id) match {
                case Some(game) =>
                  fields += ("gameentered" -> JsBoolean(value = true), "map" -> game.map.netState)
                case None =>
                  val new_game_id = nextId
                  val new_map = preloaded_map.copy(control_points = preloaded_map.control_points.map(x => (x._1, x._2.copy(team = None, control_start_time_sec = 0l)))) // TODO: allow to select map on creation
                //println(new_map)
                  val new_game = new TacticGame(new_game_id, map = new_map, count = mutable.HashMap(1 -> 0, 2 -> 0))
                  games += (new_game_id -> new_game)
                  addNewPlayerToGame(client_id, new_game, None)
                  fields += ("gameentered" -> JsBoolean(value = true), "map" -> new_map.netState)
              }
            } else if(data.join.nonEmpty) {
              games_by_clientid.get(client_id) match {
                case Some(game) =>
                  fields += ("gameentered" -> JsBoolean(value = true), "map" -> game.map.netState)
                case None =>
                  val JoinGame(game_id, team) = joinGame(data.join.get)
                  games.get(game_id) match {
                    case Some(game) =>
                      addNewPlayerToGame(client_id, game, team)
                      fields += ("gameentered" -> JsBoolean(value = true), "map" -> game.map.netState)
                    case None =>
                      val new_game_id = nextId
                      val new_map = loadMap(map_name) // TODO: allow to select map on creation
                    val new_game = new TacticGame(new_game_id, map = new_map, count = mutable.HashMap(1 -> 0, 2 -> 0))
                      games += (new_game_id -> new_game)
                      addNewPlayerToGame(client_id, new_game, None)
                      fields += ("gameentered" -> JsBoolean(value = true), "map" -> new_map.netState)
                  }
              }
            } else {
              games_by_clientid.get(client_id) match {
                case Some(game @ TacticGame(_, players, bullets, map, _)) =>
                  //println(game)
                  if(!game.isStarted && data.startgame.exists(x => x)) {
                    game.startGame()
                    stuffForClients(game.players.keys.toSeq).foreach(_.game_stats_update_required = true)
                  }
                  val client_players = players(client_id)
                  if(data.sendmap.exists(x => x)) fields += ("map" -> map.netState)
                  if(data.cps_infos_received.exists(x => x)) client_stuff.control_points_update_required = false
                  if(data.gs_update_received.exists(x => x)) client_stuff.game_stats_update_required = false
                  tacticClientData(data) match {
                    case Some(TacticClientData(player_num, destination, pov, fire_toggle, clear_destinations)) =>
                      val player = client_players(player_num)
                      if(clear_destinations) player.ds.clear()
                      destination.foreach(d => {
                        val last_position = player.ds.lastOption.getOrElse(player.coord)
                        if(d.dist2(last_position) > path_finding_radius*path_finding_radius) {
                          player.ds ++= path_finder.findPath(last_position, d)
                        } else {
                          player.ds += d
                        }
                        fields += ("dests_cleared" -> JsBoolean(value = true))
                      })
                      pov.foreach(p => player.pov = (p - player.coord).n)
                      fire_toggle.foreach(ft => {
                        player.fire_toggle = ft
                        fields += ("fire_toggle_set" -> JsBoolean(value = true))
                      })
                    case None =>
                      println(s"[server] failed to parse tacticClientData from $received_data")
                  }
                case None =>
              }
            }
          case JsError(error) =>
            println(s"[server] failed to parse client data $received_data: $error")
        }
      case UdpClientDisconnected(client_id) =>
        games_by_clientid.get(client_id) match {
          case Some(TacticGame(game_id, players, bullets, _, _)) =>
            games_by_clientid -= client_id
            for {
              client_players <- players.get(client_id)
              head_player <- client_players.headOption
              team_numbers <- numbers_in_teams.get(head_player.team)
              if head_player.number_in_team >= 0 && head_player.number_in_team < team_numbers.length
            } {
              team_numbers(head_player.number_in_team) = false
            }
            players -= client_id
            if(players.size == 0) {
              games -= game_id
            } else {
              stuffForClients(players.keys.toSeq).foreach(_.game_stats_update_required = true)
            }
          case None =>
        }
    }
  }

  // update state
  actionStaticPeriod(10) {
    val all_games = games.values
    val nonfinished_games = all_games.filter(!_.isFinished)
    nonfinished_games.foreach {
      case game @ TacticGame(_, players, bullets, map, _) =>
        players.values.flatten.foreach(p => {
          p.ds.headOption.foreach(d => {
            if(d.dist2(p.coord) > human_speed*human_speed) {
              val new_coord = p.coord + (d - p.coord).n*human_speed
              if(map.isCoordCorrect(new_coord, human_size) && (game.isStarted || map.isInsideSafeZone(new_coord))) {
                p.coord = new_coord
                if(p.isAlive) {
                  map.control_points.values.find(cp => (cp.team.isEmpty || cp.team.exists(cpt => cpt != p.team)) && coordOnArea(p.coord, cp.area)).foreach(cp => {
                    cp.team = Some(p.team)
                    cp.control_start_time_sec = System.currentTimeMillis()/1000
                    stuffForClients(players.keys.toSeq).foreach(_.control_points_update_required = true)
                  })
                }
              } else p.ds.clear()
            } else p.ds.remove(0)
          })
          if(p.canShoot && !map.isInsideSafeZone(p.coord)) {
            players
              .values
              .flatten
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
              .flatten
              .filter(p =>
                p.team != b.shooter.team &&
                isBodyHit(b.prev_coord, b.coord, p.coord) &&
                !map.isInsideSafeZone(p.coord))
            if (damaged_players.nonEmpty) {
              damaged_players.foreach(p => {
                val chance = map.chanceToHit(b.shooter.coord,
                                             b.shooter.pov,
                                             b.shooter.isMoving,
                                             p.coord,
                                             p.isMoving,
                                             p.pov)
                if(math.random < chance) {
                  p.health -= bullet_damage
                  if (p.health <= 0) {
                    p.deaths += 1
                    b.shooter.wins += 1
                    stuffForClients(players.keys.toSeq).foreach(_.game_stats_update_required = true)
                  }
                }
              })
              b.count = 0
            }
          }
        })
        bullets --= bullets.filter(b => b.count <= 0)
        game.map.control_points.values.foreach {
          case cp =>
            cp.team match {
              case Some(t) =>
                if(System.currentTimeMillis()/1000 - cp.control_start_time_sec > control_time_length_sec) {
                  game.count(t) = game.count.getOrElse(t, 0) + 1
                  cp.control_start_time_sec = System.currentTimeMillis()/1000
                  stuffForClients(players.keys.toSeq).foreach(x => {
                    x.game_stats_update_required = true
                    x.control_points_update_required = true
                  })
                }
              case None =>
            }
        }
    }
  }

  // send data
  actionStaticPeriod(50) {
    games.values.foreach {
      case game @ TacticGame(_, players, bullets, map, count) =>
        players.foreach {
          case (id, client) =>
            val (fields, player_data) = clientBuilderAndStuff(id)
            val all_players = players.values.flatten
            val (your_players, other_players) = all_players.partition(_.id == id)
            fields += ("yours" -> JsArray(your_players.map(_.netState).toSeq))
            val your_team = your_players.head.team
            val your_team_players = all_players.filter(_.team == your_team).toSeq
            val others = other_players
              .withFilter(x => x.team == your_team || your_team_players.exists(y => map.isCoordVisibleOrAudible(x.coord, y.coord, y.pov, is_moving = x.ds.nonEmpty/*true*/, human_audibility_radius)))
              .map(_.netState)
              .toList
            if(others.nonEmpty) fields += ("others" -> JsArray(others))
            val (your_bullets, other_bullets) = bullets
              .filter(b => your_team_players.exists(y => map.isCoordVisibleOrAudible(b.coord, y.coord, y.pov, is_moving = true, bullet_audibility_radius)))
              .partition(_.shooter.id == id)
            if (your_bullets.nonEmpty) fields += ("your_bullets" -> JsArray(your_bullets.map(_.netState)))
            if (other_bullets.nonEmpty) fields += ("other_bullets" -> JsArray(other_bullets.map(_.netState)))
            if(player_data.control_points_update_required) fields += ("cps_infos" -> JsArray(map.control_points.values.map(_.netState).toSeq))
            if(player_data.game_stats_update_required) {
              //println(game.gameStats)
              fields += ("gs" -> game.gameStats(your_team).netState)
            }
            val data = JsObject(fields.toList)
            server.sendToClient(id, data)
            fields.clear()
        }
    }
    nonEmptyClientBuilders.foreach(x => {
      server.sendToClient(x._1, JsObject(x._2.toList))
      x._2.clear()
    })
  }
}
