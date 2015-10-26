package com.github.dunnololda.scageprojects

import java.io.FileOutputStream

import com.github.dunnololda.scage.ScageLib._
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
//import java.awt.GraphicsEnvironment

package object simpleshooter {
  val main_title_printer = new ScageMessage(max_font_size = 50)
  val settings_title_printer = new ScageMessage(max_font_size = 30)
  val window_settings_title_printer = new ScageMessage(max_font_size = 30)
  val help_printer = new ScageMessage(max_font_size = 15)

  val host = "localhost"
  val port = 10000

  val human_size = 20f  // human_size is 1 meter
  val human_speed = 12000f/60/60/1000*10*human_size  // 12 km/h in px / 10 msec
  val bullet_speed = 120f/1000*10*human_size  // 120 m/sec in px / 10msec
  val near_wall_area = human_size*2.5f  // 2.5 m
  val pov_distance = 60*human_size
  val human_audibility_radius = 6*human_size
  val bullet_audibility_radius = 60*human_size
  val path_finding_radius = 15*human_size

  val bullet_count = 50  // 24 px / 10msec * 50 = 1200 px = 60 m
  val bullet_damage = 100

  val map_width = 100*human_size  // 100 m
  val map_height = 100*human_size
  /*lazy val gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
  lazy val game_window_width = gd.getDisplayMode().getWidth()
  lazy val game_window_height = gd.getDisplayMode().getHeight()*/
  lazy val default_window_width = 640
  lazy val default_window_height = 480

  val bullet_size = 3f
  val pov_angle = 90
  val single_fire_pace = 600 // 600 msec/bullet = 60000 msec / 100 bullet = 100 bullet/min -> AK, single mode
  val rapid_fire_pace = 100 // 100 msec/bullet = 60000 msec / 600 bullet = 600 bullet/min -> AK, rapid mode
  val reload_time = 5000  // 5 sec to swap magazines
  val magazine = 30 // 30 rounds in AK's magazine
  val max_bullets = 90  // three magazines
  val game_period_length_sec = 15l*60  // 15 minutes
  val control_time_length_sec = 15l  // 15 seconds to control to receive 1 point

  val map_edges = List(Vec(-map_width/2, map_height/2), Vec(map_width/2, map_height/2), Vec(map_width/2, -map_height/2), Vec(-map_width/2, -map_height/2), Vec(-map_width/2, map_height/2))

  case class Client(id:Long, var coord:Vec, var health:Int, var wins:Int, var deaths:Int, var visible:Boolean) {
    def netState:JsObject = Json.obj(
      "id" -> id,
      "coord" -> Json.obj("x" -> coord.x, "y" -> coord.y),
      "health" -> health,
      "wins"  -> wins,
      "deaths"  -> deaths,
      "visible" -> visible)
  }

  case class ClientData(up:Boolean, left:Boolean, down:Boolean, right:Boolean, shoots:List[Vec])

  case class ServerData(you:Client, others:List[Client], your_bullets:List[Vec], other_bullets:List[Vec])

  def serverData(data:ShooterServerData):Option[ServerData] = {
    for {
      you <- data.you
      others <- data.others
      your_bullets <- data.your_bulltes
      other_bullets <- data.other_bullets
    } yield ServerData(you, others, your_bullets, other_bullets)
  }

  case class Bullet(dir:Vec, shooter:Client, var coord:Vec) {
    var count:Int = bullet_count
    def netState = Json.obj("x" -> coord.x, "y" -> coord.y)
  }

  case class TacticServerPlayerData(id:Long, n:Int, t:Int, nit:Int, c:Vec, ds:List[Vec], p:Vec, bs:Int, r:Boolean, hp:Int, w:Int, d:Int)

  case class TacticServerPlayer(
                          id:Long,
                          number:Int,
                          team:Int,
                          number_in_team:Int,
                          var coord:Vec,
                          var pov:Vec,
                          var health:Int,
                          var wins:Int,
                          var deaths:Int) {
    val ds = ArrayBuffer[Vec]()
    def isMoving = ds.nonEmpty

    var last_bullet_shot = 0l

    private var _bullets = max_bullets
    private var reload_start_time = 0l

    var fire_toggle = 0

    def shootBullet(dir:Vec):TacticServerBullet = {
      _bullets -= 1
      if(_bullets > 0 && _bullets % magazine == 0) reload_start_time = System.currentTimeMillis()
      last_bullet_shot = System.currentTimeMillis()
      val init_coord = coord + dir*(human_size/2+1)
      val bullet_id:Long = nextId
      TacticServerBullet(bullet_id, dir, this, init_coord, init_coord)
    }
    def bullets = _bullets
    def replenishAmmo() {_bullets = max_bullets}
    def isReloading:Boolean = {
      System.currentTimeMillis() - reload_start_time < reload_time
    }
    def canShoot:Boolean = {
      health > 0 && _bullets > 0 && !isReloading &&
        (fire_toggle == 2 && System.currentTimeMillis() - last_bullet_shot > rapid_fire_pace ||
         fire_toggle == 1 && System.currentTimeMillis() - last_bullet_shot > single_fire_pace)
    }

    def isDead = health <= 0
    def isAlive = health > 0

    def netState:JsObject = Json.obj(
      "id"    -> id,
      "n"     -> number,
      "t"     -> team,
      "nit"   -> number_in_team,
      "c" -> Json.obj("x"  -> coord.x, "y"  -> coord.y),
      "ds"    -> ds.toList.map(d => Json.obj("x" -> d.x, "y" -> d.y)),
      "p"     -> Json.obj("x" -> pov.x, "y" -> pov.y),
      "bs"    -> _bullets,
      "r"     -> isReloading,
      "hp"    -> health,
      "w"     -> wins,
      "d"     -> deaths)
  }

  case class TacticClientPlayer(
     id:Long,
     number_of_fighter_in_group:Int,
     team:Int,
     number_of_group_in_team:Int,
     coord:Vec,
     destinations:List[Vec],
     pov:Vec,
     bullets:Int,
     is_reloading:Boolean,
     health:Int,
     wins:Int,
     deaths:Int
  ) {
    def isDead = health <= 0
    def isMoving = destinations.nonEmpty
  }

  def tacticClientPlayer(message:TacticServerPlayerData):TacticClientPlayer = {
    TacticClientPlayer(
      id = message.id,
      number_of_fighter_in_group = message.n,
      team = message.t,
      number_of_group_in_team = message.nit,
      coord = message.c,
      destinations = message.ds,
      pov = message.p,
      bullets = message.bs,
      is_reloading = message.r,
      health = message.hp,
      wins = message.w,
      deaths = message.d
    )
  }

  case class TacticClientData(player_num:Int, destination:Option[Vec], pov: Option[Vec], fire_toggle:Option[Int], clear_destinations:Boolean)

  def tacticClientData(message:TacticShooterClientData):Option[TacticClientData] = {
    for {
      player_num <- message.pn
      destination = message.d
      pov = message.pov
      fire_toggle = message.ft
      clear_destinations = message.cleardest.getOrElse(false)
    } yield TacticClientData(
      player_num,
      destination,
      pov,
      fire_toggle,
      clear_destinations
    )
  }

  case class TacticServerData(
                               yours:List[TacticClientPlayer],
                               others:List[TacticClientPlayer],
                               your_bullets:List[TacticClientBullet],
                               other_bullets:List[TacticClientBullet],
                               receive_moment:Long
  )

  def tacticServerData(message:TacticShooterServerData, receive_moment:Long):TacticServerData = {
    val yours = message.yours.getOrElse(Nil).map(m => tacticClientPlayer(m))
    val others = message.others.getOrElse(Nil).map(m => tacticClientPlayer(m))
    val your_bullets = message.your_bullets.getOrElse(Nil).map(m => tacticClientBullet(m))
    val other_bullets = message.other_bullets.getOrElse(Nil).map(m => tacticClientBullet(m))
    TacticServerData(yours, others, your_bullets, other_bullets, receive_moment)
  }

  case class Wall(from:Vec, to:Vec) {
    val netState = Json.obj("from" -> Json.obj("x" -> from.x, "y" -> from.y), "to" -> Json.obj("x" -> to.x, "y" -> to.y))
  }

  case class TacticServerBulletData(id:Long, pid:Long, pn:Int, pt:Int, c:Vec)

  case class TacticServerBullet(id:Long, dir:Vec, shooter:TacticServerPlayer, var prev_coord:Vec, var coord:Vec) {
    var count:Int = bullet_count
    def netState = Json.obj(
      "id" -> id,
      "pid" -> shooter.id,
      "pn" -> shooter.number,
      "pt" -> shooter.team,
      "c" -> Json.obj("x" -> coord.x, "y" -> coord.y)
    )
  }

  case class TacticClientBullet(id:Long, player_id:Long, player_number:Int, player_team:Int, coord:Vec)

  def tacticClientBullet(message:TacticServerBulletData):TacticClientBullet = {
    TacticClientBullet(
      id = message.id,
      player_id = message.pid,
      player_number = message.pn,
      player_team = message.pt,
      coord = message.c)
  }

  case class PlayerStatsData(t:Int, nit:Int, n:Int, w:Int, d:Int)

  case class PlayerStats(team:Int, number_in_team:Int, number:Int, wins:Int, deaths:Int) {
    val netState = Json.obj(
      "t"   -> team,
      "nit" -> number_in_team,
      "n"   -> number,
      "w"   -> wins,
      "d"   -> deaths
    )
  }

  def playerStats(message:PlayerStatsData):PlayerStats = {
    PlayerStats(
      team           = message.t,
      number_in_team = message.nit,
      number         = message.n,
      wins           = message.w,
      deaths         = message.d
    )
  }

  case class TeamStatsData(t:Int, tp:Int, ps:List[PlayerStatsData])

  case class TeamStats(team:Int, team_points:Int, players_stats:List[PlayerStats]) {
    val netState = Json.obj(
      "t" -> team,
      "tp" -> team_points,
      "ps" -> players_stats.map(_.netState)
    )
  }

  def teamStats(message:TeamStatsData):TeamStats = {
    TeamStats(
      team = message.t,
      team_points = message.tp,
      players_stats = message.ps.map(x => playerStats(x))
    )
  }

  case class GameStatsData(ts:List[TeamStatsData], gs:Option[Long], yt:Int)

  case class GameStats(teams_stats:List[TeamStats], game_start_moment_sec:Option[Long], your_team:Int) {
    private val fields = ArrayBuffer[(String, JsValue)]()
    fields += ("ts" -> JsArray(teams_stats.map(_.netState)))
    game_start_moment_sec.foreach(gs => fields += ("gs" -> JsNumber(gs)))
    fields += ("yt" -> JsNumber(your_team))
    val netState = JsObject(fields)
  }

  def gameStats(message:GameStatsData):GameStats = {
    GameStats(
      teams_stats = message.ts.map(x => teamStats(x)),
      game_start_moment_sec = message.gs,
      your_team = message.yt
    )
  }

  case class TacticClientStuff(
    var control_points_update_required:Boolean = false,
    var game_stats_update_required:Boolean = false
  )

  case class TacticGame(game_id:Int,
                        players:mutable.HashMap[Long, List[TacticServerPlayer]] = mutable.HashMap[Long, List[TacticServerPlayer]](),  // client_id -> list of players
                        bullets:ArrayBuffer[TacticServerBullet] = ArrayBuffer[TacticServerBullet](),
                        map:GameMap,
                        count:mutable.HashMap[Int, Int]) {
    private var game_start_moment_sec = 0l
    private var game_started = false
    def startGame() {
      game_started = true
      game_start_moment_sec = System.currentTimeMillis()/1000
    }

    def isStarted = game_started
    def isFinished = game_started && System.currentTimeMillis()/1000 - game_start_moment_sec > game_period_length_sec

    def gameStats(your_team:Int):GameStats = {
      val teams_stats = count.toList.map {
        case (team, points) =>
          val player_stats = players.values.flatten.filter(_.team == team).map(p => {
            PlayerStats(p.team, p.number_in_team, p.number, p.wins, p.deaths)
          }).toList
          TeamStats(team, points, player_stats)
      }
      if(!game_started) {
        GameStats(teams_stats, game_start_moment_sec = None, your_team)
      } else {
        GameStats(teams_stats, game_start_moment_sec = Some(game_start_moment_sec), your_team)
      }
    }
  }

  case class GameInfoData(gid:Int, t1p:Int, t2p:Int)

  case class GameInfo(game_id:Int, team1_players:Int, team2_players:Int) {
    val players = team1_players+team2_players
    val netState = Json.obj(
      "gid" -> game_id,
      "t1p" -> team1_players,
      "t2p" -> team2_players
    )
  }

  case class JoinGameData(gid:Int, t:Option[Int])

  case class JoinGame(game_id:Int, team:Option[Int]) {
    val netState = {
      val fields = ArrayBuffer[(String, JsValue)]()
      fields += ("gid" -> JsNumber(game_id))
      team.foreach(t => fields += ("t" -> JsNumber(t)))
      JsObject(fields)
    }
  }

  def joinGame(message:JoinGameData):JoinGame = {
    JoinGame(
      game_id = message.gid,
      team    =  message.t
    )
  }

  def gameInfo(message:GameInfoData):GameInfo = {
    GameInfo(message.gid, message.t1p, message.t2p)
  }

  def gamesList(message:TacticShooterServerData):List[GameInfo] = {
    message.gameslist.map(gs => gs.map(m => gameInfo(m))).getOrElse(Nil)
  }
  
  case class ControlPointData(n:Int, t:Option[Int], cst:Long, area:List[Vec])

  case class ControlPoint(number:Int, var team:Option[Int], var control_start_time_sec:Long, area:List[Vec]) {
    val area_center = Vec(area.map(_.x).sum/area.length, area.map(_.y).sum/area.length)

    def controlPointColor(your_team:Int):ScageColor = {
      team match {
        case Some(cp_team) => if(cp_team != your_team) RED else GREEN
        case None => GRAY
      }
    }

    def netState = {
      val fields = ArrayBuffer[(String, JsValue)]()
      fields += ("n" -> JsNumber(number))
      if(team.nonEmpty) fields += ("t" -> JsNumber(team.get))
      fields += ("cst" -> JsNumber(control_start_time_sec))
      fields += ("area" -> JsArray(area.map(p => Json.obj("x" -> p.x, "y" -> p.y))))
      JsObject(fields)
    }
  }

  def controlPoint(message:ControlPointData):ControlPoint = {
    ControlPoint(
      number = message.n,
      team = message.t,
      control_start_time_sec = message.cst,
      area = message.area
    )
  }

  case class ControlPointInfo(number:Int, team:Option[Int], control_start_time:Long)

  def controlPointInfos(message:List[ControlPointData]):List[ControlPointInfo] = {
    message.map(x => {
      ControlPointInfo(
        number = x.n,
        team = x.t,
        control_start_time = x.cst
      )
    })
  }

  class GameMapPathFinder(map:GameMap) {
    val tiles_on_map = Array.fill((map_width/human_size).toInt+1, (map_height/human_size).toInt+1)(false)
    for {
      (row, y) <- tiles_on_map.zipWithIndex
      (elem, x) <- row.zipWithIndex
      coord = Vec(-map_width/2 + x*human_size, -map_height/2 + y*human_size)
    } {
      if(!map.isCoordCorrectForPathFinder(coord, human_size)) tiles_on_map(x)(y) = true
    }
    private val path_finder = PathFinder(
      (map_width/human_size).toInt+1,
      (map_height/human_size).toInt+1,
      is_blocked = (x,y) => tiles_on_map(x)(y))
    def findPath(from:Vec, to:Vec):List[Vec] = {
      val fx = ((from.x + map_width/2)/human_size).toInt
      val fy = ((from.y + map_height/2)/human_size).toInt
      val tx = ((to.x + map_width/2)/human_size).toInt
      val ty = ((to.y + map_height/2)/human_size).toInt
      val path = path_finder.findPath(Vec(fx, fy), Vec(tx, ty)).map(p => Vec(-map_width/2 + p.x*human_size, -map_height/2 + p.y*human_size)).toList
      reducePath(from :: path ::: to :: Nil)
    }

    private def reducePath(path:List[Vec]):List[Vec] = {
      val path_buffer = path.toBuffer
      path.sliding(3).foreach {
        case List(a, b, c) =>
          val dir1 = (b - a).n
          val dir2 = (c - b).n
          if(dir1 == dir2) path_buffer -= b
        case _ =>
      }
      path_buffer.toList
    }
  }

  case class TacticShooterServerData(gameslist:Option[List[GameInfoData]],
                                     gameentered:Option[Boolean],
                                     map:Option[GameMapData],
                                     dests_cleared:Option[Boolean],
                                     fire_toggle_set:Option[Boolean],
                                     yours:Option[List[TacticServerPlayerData]],
                                     others:Option[List[TacticServerPlayerData]], 
                                     your_bullets:Option[List[TacticServerBulletData]], 
                                     other_bullets:Option[List[TacticServerBulletData]],
                                     cps_infos:Option[List[ControlPointData]],
                                     gs:Option[GameStatsData])

  case class GameMapData(ws:List[Wall], szs:List[List[Vec]], cps:List[ControlPointData])

  case class GameMap(walls:List[Wall] = Nil, safe_zones:List[List[Vec]] = Nil, control_points:Map[Int, ControlPoint] = Map()) {
    def isEmpty:Boolean = walls.isEmpty && safe_zones.isEmpty && control_points.isEmpty

    def isCoordCorrect(coord:Vec, body_size:Float):Boolean = {
      isCoordInsideMapBorders(coord) && walls.forall(w => !isCoordNearWall(coord, w, body_size, 90))
    }

    def isCoordCorrectForPathFinder(coord:Vec, body_size:Float):Boolean = {
      isCoordInsideMapBorders(coord) && walls.forall(w => !isCoordNearWall(coord, w, body_size, 135))
    }

    def isPathCorrect(from:Vec, to:Vec, body_radius:Float):Boolean = {
      walls.forall(w => !areLinesIntersect(from, to, w.from, w.to)) && isCoordCorrect(to, body_radius)
    }

    def randomHumanCoord:Vec = {
      val coord = Vec(math.random*map_width, math.random*map_height)
      if (isCoordCorrect(coord, human_size)) coord
      else randomHumanCoord
    }

    def randomHumanCoordNear(near:Vec, radius:Float):Vec = {
      val dir = Vec(math.random, math.random).n
      val coord = near + dir*radius
      if (isCoordCorrect(coord, human_size)) coord
      else randomHumanCoordNear(near, radius)
    }

    def randomHumanCoordInsideArea(area:List[Vec]):Vec = {
      val area_center = Vec(area.map(_.x).sum/area.length, area.map(_.y).sum/area.length)
      val random_corner = area((math.random*area.length).toInt)
      val coord = area_center + (random_corner - area_center).n*(math.random*random_corner.dist(area_center))
      if (isCoordCorrect(coord, human_size)) coord
      else randomHumanCoordInsideArea(area)
    }

    def respawnCoord(team:Int):Vec = {
      if(safe_zones.length == 0) {
        randomHumanCoord
      } else if(safe_zones.length == 0) {
        randomHumanCoordInsideArea(safe_zones.head)
      } else {
        val team1_area = safe_zones.head
        val team2_area = safe_zones(1)
        team match {
          case 1 => randomHumanCoordInsideArea(team1_area)
          case 2 => randomHumanCoordInsideArea(team2_area)
          case _ => randomHumanCoordInsideArea(team1_area)
        }
      }
    }

    def isCoordVisible(coord:Vec, from:Vec, pov:Vec):Boolean = {
      isCoordInsidePov(coord, from, pov) && walls.forall(w => !areLinesIntersect(from, coord, w.from, w.to))
    }

    def isCoordVisibleOrAudible(coord:Vec, from:Vec, pov:Vec, is_moving:Boolean, audibility_radius:Float):Boolean = {
      is_moving && coord.dist2(from) <= audibility_radius * audibility_radius ||
      isCoordInsidePov(coord, from, pov) && walls.forall(w => !areLinesIntersect(from, coord, w.from, w.to))
    }

    def isInsideSafeZone(coord:Vec):Boolean = safe_zones.exists(
      sz => coordOnArea(coord, sz)
    )

    def hitChanceModification(shooter_coord:Vec, target_coord:Vec):Boolean = {
      walls.exists(w => {
        w.from.dist2(target_coord) < near_wall_area*near_wall_area && areLinesIntersect(shooter_coord, target_coord, w.from, w.from + w.from - w.to) ||
          w.to.dist2(target_coord) < near_wall_area*near_wall_area && areLinesIntersect(shooter_coord, target_coord, w.to, w.to + w.to - w.from)
      })
    }

    def chanceToHit(shooter_coord:Vec,
                    shooter_pov:Vec,
                    shooter_moving:Boolean,
                    target_coord:Vec,
                    target_moving:Boolean,
                    target_pov:Vec):Float = {
      if(!isCoordVisible(target_coord, shooter_coord, shooter_pov)) 0f
      else {
        val d = target_coord.dist(shooter_coord)
        var result = -0.001125f*d + 0.6125f
        if(shooter_moving) result /= 3f
        if(target_moving) result /= 2f
        if(!isCoordVisible(shooter_coord, target_coord, target_pov)) result *= 2f
        if(hitChanceModification(shooter_coord, target_coord)) result /= 2f
        if(result > 1f) 1f
        else if(result < 0) 0f
        else result
      }
    }

    val netState = Json.obj(
      "ws" -> walls.map(w => w.netState),
      "szs" -> safe_zones.map(sz => sz.map(p => Json.obj("x" -> p.x, "y" -> p.y))),
      "cps" -> control_points.map(cp => cp._2.netState)
    )
  }

  def saveMap(map_name:String, walls:Seq[Wall], safe_zones:Seq[Seq[Vec]], control_points:Seq[(Int, ControlPoint)]) {
    val fos = new FileOutputStream(map_name)
    if(walls.nonEmpty) {
      fos.write("walls\n".getBytes)
      walls.foreach(w => {
        fos.write(s"${w.from.x} ${w.from.y} ${w.to.x} ${w.to.y}\n".getBytes)
      })
    }
    if(safe_zones.nonEmpty) {
      fos.write("safe zones\n".getBytes)
      safe_zones.foreach(sz => {
        fos.write(sz.map(p => s"${p.x} ${p.y}").mkString("", " ", "\n").getBytes)
      })
    }
    if(control_points.nonEmpty) {
      fos.write("control points\n".getBytes)
       control_points.foreach {
         case (number, ControlPoint(_, _, _, area)) =>
           fos.write(area.map(p => s"${p.x} ${p.y}").mkString("", " ", "\n").getBytes)
       }
    }
    fos.close()
  }

  def loadMap(map_name:String):GameMap = {
    def tryFloat(str:String):Boolean = {
      try {
        str.toFloat
        true
      } catch {
        case e:Exception => false
      }
    }

    val walls = ArrayBuffer[Wall]()
    val safe_zones = ArrayBuffer[List[Vec]]()
    val control_points = ArrayBuffer[(Int, ControlPoint)]()
    var mode = ""
    try {
      for {
        line <- io.Source.fromFile(map_name).getLines()
        if !line.startsWith("#")
      } {
        if(line == "walls")               mode = "walls"
        else if(line == "safe zones")     mode = "safe zones"
        else if(line == "control points") mode = "control points"
        else {
          mode match {
            case "walls" =>
              val coords = line.split(" ")
              if(coords.length == 4 && coords.forall(c => tryFloat(c))) {
                walls += Wall(Vec(coords(0).toFloat, coords(1).toFloat), Vec(coords(2).toFloat, coords(3).toFloat))
              }
            case "safe zones" =>
              val coords = line.split(" ")
              if(coords.length % 2 == 0 && coords.forall(c => tryFloat(c))) {
                val new_safe_zone = ArrayBuffer[Vec]()
                coords.grouped(2).foreach {
                  case Array(x, y) => new_safe_zone += Vec(x.toFloat, y.toFloat)
                  case _ =>
                }
                safe_zones += new_safe_zone.toList
              }
            case "control points" =>
              val control_point_coords = line.split(" ")
              if(control_point_coords.length % 2 == 0 && control_point_coords.forall(c => tryFloat(c))) {
                val new_control_point_coords = ArrayBuffer[Vec]()
                control_point_coords.grouped(2).foreach {
                  case Array(x, y) => new_control_point_coords += Vec(x.toFloat, y.toFloat)
                  case _ =>
                }
                val number = control_points.length
                control_points += (number -> ControlPoint(number, None, 0l, new_control_point_coords.toList))
              }
            case _ =>
          }
        }
      }
      //println(walls)
      GameMap(walls.toList, safe_zones.toList, control_points.toMap)
    } catch {
      case e:Exception =>
        throw e
        //GameMap(Nil, Nil, Map())
    }
  }

  def gameMap(mapData:MapData):GameMap = {
    GameMap(
      walls = mapData.ws,
      safe_zones = mapData.szs,
      control_points = mapData.cps.map(cp => {
        (cp.number, cp)
      }).toMap
    )
  }

  def gameMap(mapData:GameMapData):GameMap = {
    GameMap(
      walls = mapData.ws,
      safe_zones = mapData.szs,
      control_points = mapData.cps.map(cpd => {
        val cp = controlPoint(cpd)
        (cp.number,  cp)
      }).toMap
    )
  }

  def isCoordNearWall(coord:Vec, wall:Wall, body_size:Float, angle:Float):Boolean = {
    val one = (wall.to - wall.from).rotateDeg(angle).n*body_size/2 + wall.from
    val two = (wall.to - wall.from).rotateDeg(-angle).n*body_size/2 + wall.from
    val three = (wall.from - wall.to).rotateDeg(angle).n*body_size/2 + wall.to
    val four = (wall.from - wall.to).rotateDeg(-angle).n*body_size/2 + wall.to
    val area = List(one, two, three, four)
    coordOnArea(coord, area)
  }

  def isCoordInsideMapBorders(coord:Vec):Boolean = {
    coord.x >= -map_width / 2 && coord.x <= map_width / 2 && coord.y >= -map_height / 2 && coord.y <= map_height / 2
  }

  def isCoordInsidePov(coord:Vec, from:Vec, pov:Vec):Boolean = {
    coord.dist2(from) < pov_distance*pov_distance && math.abs((coord - from).deg(pov)) < pov_angle
  }

  def isBodyHit(from:Vec, to:Vec, body_position:Vec):Boolean = {
    val a = body_position + (to - from).rotateDeg(90)
    val b = body_position + (to - from).rotateDeg(-90)
    areLinesIntersect(from ,to, a, b) || body_position.dist2(to) < human_size*human_size/4
  }

  def isCoordVisible(coord:Vec, from:Vec, walls:Seq[Wall]):Boolean = {
    walls.forall(w => {
      !areLinesIntersect(from, coord, w.from, w.to)
    })
  }

  def isWallVisible(wall:Wall, from:Vec, other_walls:Seq[Wall]):Boolean = {
    val middle = (wall.to - wall.from).n*(wall.to.dist(wall.from)/2) + wall.from
    isCoordVisible(wall.from, from, other_walls) || isCoordVisible(middle, from, other_walls) || isCoordVisible(wall.to, from, other_walls)
  }

  def outsideCoord(coord:Vec):Vec = {
    def checkC(c:Float, from:Float, to:Float):Float = {
      val dist = to - from
      if(c >= to) checkC(c - dist, from, to)
      else if(c < from) checkC(c + dist, from, to)
      else c
    }
    val x = checkC(coord.x, 0, map_width)
    val y = checkC(coord.y, 0, map_height)
    Vec(x, y)
  }

  /*def povTriangle(coord:Vec, pov:Vec, distance:Float, angle:Float):List[Vec] = {
    val axis = pov*distance
    val one = coord + axis.rotateDeg(angle)
    val two = coord + axis.rotateDeg(-angle)
    List(coord, one, two, coord)
  }*/

  def messageAreaCentered(message:Any, coord:Vec, printer:ScageMessage = ScageMessage):List[Vec] = {
    val Vec(w, h) = messageBounds(message)
    val Vec(x, y) = coord
    List(Vec(x-w/2, y+h/2), Vec(x+w/2, y+h/2), Vec(x+w/2, y-h/2), Vec(x-w/2, y-h/2))
  }
  def messageArea(message:Any, coord:Vec, printer:ScageMessage = ScageMessage):List[Vec] = {
    val Vec(w, h) = messageBounds(message)
    val Vec(x, y) = coord
    List(Vec(x, y), Vec(x, y+h), Vec(x+w, y+h), Vec(x+w, y))
  }

  def createMenuItems(menu_items:List[(String, () => Vec,  () => ScageColor, () => Any)],
                      printer:ScageMessage = ScageMessage):List[(String, () => Vec, () => List[Vec], () => ScageColor, () => Any)] = {
    menu_items.map {
      case (title, coord, color, action) => (title, coord, () => messageAreaCentered(title, coord(), printer), color, action)
    }
  }

  def bresenham(from:Vec, to:Vec, a:Float, b:Float, cell_size:Float):Stream[(Int, Int)] = {
    val xstart = ((from.x - a)/cell_size).toInt
    val ystart = ((from.y - b)/cell_size).toInt
    val xend = ((to.x - a)/cell_size).toInt
    val yend = ((to.y - b)/cell_size).toInt

    val dx = xend - xstart
    val dy = yend - ystart

    val incx = if(dx > 0) 1 else -1
    val incy = if(dy > 0) 1 else -1

    val adx = math.abs(dx)
    val ady = math.abs(dy)

    val (pdx, pdy, es, el) = if (adx > ady) (incx, 0, ady, adx) else (0, incy, adx, ady)
    def pointsFrom(t:Int, err:Int, x:Int, y:Int):Stream[(Int, Int)] = {
      if (t >= el) Stream((x, y))
      else {
        val next_err_1 = err - es
        if (next_err_1 < 0) {
          Stream((x, y), ( x+incx, y)) #::: pointsFrom(t+1, next_err_1+el, x+incx, y+incy)
        } else {
          (x, y) #:: pointsFrom(t+1, next_err_1, x+pdx, y+pdy)
        }
      }
    }
    pointsFrom(0, el/2, xstart, ystart)
  }
}
