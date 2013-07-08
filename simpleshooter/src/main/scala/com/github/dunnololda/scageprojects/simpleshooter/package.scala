package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState}
import collection.mutable.ArrayBuffer
import scala.collection.mutable
import java.io.FileOutputStream
//import java.awt.GraphicsEnvironment

package object simpleshooter {
  val main_title_printer = new ScageMessage(max_font_size = 50)
  val settings_title_printer = new ScageMessage(max_font_size = 30)
  val window_settings_title_printer = new ScageMessage(max_font_size = 30)
  val help_printer = new ScageMessage(max_font_size = 15)

  val host = "fzeulf.netris.ru"
  val port = 10000

  val human_size = 20f  // human_size is 1 meter
  val human_speed = 12000f/60/60/1000*10*human_size  // 12 km/h in px / 10 msec
  val bullet_speed = 120f/1000*10*human_size  // 120 m/sec in px / 10msec
  val near_wall_area = human_size*2.5f  // 2.5 m
  val pov_distance = 60*human_size
  val human_audibility_radius = 3*human_size
  val bullet_audibility_radius = 60*human_size

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
  val pov_angle = 50
  val single_fire_pace = 600 // 600 msec/bullet = 60000 msec / 100 bullet = 100 bullet/min -> AK, single mode
  val rapid_fire_pace = 100 // 100 msec/bullet = 60000 msec / 600 bullet = 600 bullet/min -> AK, rapid mode
  val reload_time = 5000  // 5 sec to swap magazines
  val magazine = 30 // 30 rounds in AK's magazine
  val max_bullets = 90  // three magazines
  val game_period_length_sec = 15l*60  // 15 minutes
  val control_time_length_sec = 15l  // 15 seconds to control to receive 1 point

  val map_edges = List(Vec(-map_width/2, map_height/2), Vec(map_width/2, map_height/2), Vec(map_width/2, -map_height/2), Vec(-map_width/2, -map_height/2), Vec(-map_width/2, map_height/2))

  def vec(message:NetState, x:String, y:String):Vec = {
    Vec(message.value[Float](x).get, message.value[Float](y).get)
  }

  case class Client(id:Long, var coord:Vec, var health:Int, var wins:Int, var deaths:Int, var visible:Boolean) {
    def netState:NetState = NetState("id" -> id,
      "x"  -> coord.x,
      "y"  -> coord.y,
      "hp" -> health,
      "w"  -> wins,
      "d"  -> deaths,
      "v" -> visible)
  }

  def client(message:NetState):Client = {
    Client(id = message.value[Long]("id").get,
      coord = vec(message, "x", "y"),
      health = message.value[Int]("hp").get,
      wins = message.value[Int]("w").get,
      deaths = message.value[Int]("d").get,
      visible = message.value[Boolean]("v").get
    )
  }

  case class ClientData(up:Boolean, left:Boolean, down:Boolean, right:Boolean, shoots:List[Vec])

  def clientData(message:NetState):ClientData = {
    ClientData(
      up = message.valueOrDefault("up", false),
      left = message.valueOrDefault("left", false),
      down = message.valueOrDefault("down", false),
      right = message.valueOrDefault("right", false),
      shoots = message.valueOrDefault[List[NetState]]("shoots", Nil).map(x => vec(x, "x", "y"))

    )
  }

  case class ServerData(you:Client, others:List[Client], your_bullets:List[Vec], other_bullets:List[Vec])

  def serverData(message:NetState):ServerData = {
    val you = client(message.value[NetState]("you").get)
    val others = message.value[List[NetState]]("others").getOrElse(Nil).map(m => client(m))
    val your_bullets = message.value[List[NetState]]("your_bullets").getOrElse(Nil).map(m => vec(m, "x", "y"))
    val other_bullets = message.value[List[NetState]]("other_bullets").getOrElse(Nil).map(m => vec(m, "x", "y"))
    ServerData(you, others, your_bullets, other_bullets)
  }

  case class Bullet(dir:Vec, shooter:Client, var coord:Vec) {
    var count:Int = bullet_count
    def netState = NetState("x" -> coord.x, "y" -> coord.y)
  }

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

    def netState:NetState = NetState(
      "id" -> id,
      "n" -> number,
      "t" -> team,
      "nit" -> number_in_team,
      "x"  -> coord.x,
      "y"  -> coord.y,
      "ds"  -> ds.toList.map(d => NetState("x" -> d.x, "y" -> d.y)),
      "px" -> pov.x,
      "py" -> pov.y,
      "bs" -> _bullets,
      "r" -> isReloading,
      "hp" -> health,
      "w"  -> wins,
      "d"  -> deaths)
  }

  case class TacticClientPlayer(
     id:Long,
     number:Int,
     team:Int,
     number_in_team:Int,
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

  def tacticClientPlayer(message:NetState):TacticClientPlayer = {
    TacticClientPlayer(
      id = message.value[Long]("id").get,
      number = message.value[Int]("n").get,
      team = message.value[Int]("t").get,
      number_in_team = message.value[Int]("nit").get,
      coord = vec(message, "x", "y"),
      destinations = message.value[List[NetState]]("ds").getOrElse(Nil).map(m => vec(m, "x", "y")),
      pov = vec(message, "px", "py"),
      bullets = message.value[Int]("bs").get,
      is_reloading = message.value[Boolean]("r").get,
      health = message.value[Int]("hp").get,
      wins = message.value[Int]("w").get,
      deaths = message.value[Int]("d").get
    )
  }

  case class TacticClientData(player_num:Int, destination:Option[Vec], pov: Option[Vec], fire_toggle:Option[Int], clear_destinations:Boolean)

  def tacticClientData(message:NetState):TacticClientData = {
    TacticClientData(
      player_num = message.value[Int]("pn").get,
      destination = message.value[NetState]("d").map(x => vec(x, "x", "y")),
      pov = message.value[NetState]("pov").map(x => vec(x, "x", "y")),
      fire_toggle = message.value[Int]("ft"),
      clear_destinations = message.value[Boolean]("cleardest").getOrElse(false)
    )
  }

  case class TacticServerData(
                               yours:List[TacticClientPlayer],
                               others:List[TacticClientPlayer],
                               your_bullets:List[TacticClientBullet],
                               other_bullets:List[TacticClientBullet],
                               receive_moment:Long
  )

  def tacticServerData(message:NetState, receive_moment:Long):TacticServerData = {
    val yours = message.value[List[NetState]]("yours").getOrElse(Nil).map(m => tacticClientPlayer(m))
    val others = message.value[List[NetState]]("others").getOrElse(Nil).map(m => tacticClientPlayer(m))
    val your_bullets = message.value[List[NetState]]("your_bullets").getOrElse(Nil).map(m => tacticClientBullet(m))
    val other_bullets = message.value[List[NetState]]("other_bullets").getOrElse(Nil).map(m => tacticClientBullet(m))
    TacticServerData(yours, others, your_bullets, other_bullets, receive_moment)
  }

  case class Wall(from:Vec, to:Vec) {
    val netState = NetState("fromx" -> from.x, "fromy" -> from.y, "tox" -> to.x, "toy" -> to.y)
  }

  def wall(message:NetState):Wall = {
    Wall(Vec(message.value[Float]("fromx").get, message.value[Float]("fromy").get), Vec(message.value[Float]("tox").get, message.value[Float]("toy").get))
  }

  def serverWalls(message:NetState):List[Wall] = {
    message.value[List[NetState]]("walls").get.map(x => wall(x))
  }

  case class TacticServerBullet(id:Long, dir:Vec, shooter:TacticServerPlayer, var prev_coord:Vec, var coord:Vec) {
    var count:Int = bullet_count
    def netState = NetState(
      "id" -> id,
      "pid" -> shooter.id,
      "pn" -> shooter.number,
      "pt" -> shooter.team,
      "x" -> coord.x,
      "y" -> coord.y
    )
  }

  case class TacticClientBullet(id:Long, player_id:Long, player_number:Int, player_team:Int, coord:Vec)

  def tacticClientBullet(message:NetState):TacticClientBullet = {
    TacticClientBullet(
      id = message.value[Long]("id").get,
      player_id = message.value[Long]("pid").get,
      player_number = message.value[Int]("pn").get,
      player_team = message.value[Int]("pt").get,
      coord = vec(message, "x", "y"))
  }

  case class PlayerStats(team:Int, number_in_team:Int, number:Int, wins:Int, deaths:Int) {
    val netState = NetState(
      "t"   -> team,
      "nit" -> number_in_team,
      "n"   -> number,
      "w"   -> wins,
      "d"   -> deaths
    )
  }

  def playerStats(message:NetState):PlayerStats = {
    PlayerStats(
      team           = message.value[Int]("t").get,
      number_in_team = message.value[Int]("nit").get,
      number         = message.value[Int]("n").get,
      wins           = message.value[Int]("w").get,
      deaths         = message.value[Int]("d").get
    )
  }

  case class TeamStats(team:Int, team_points:Int, players_stats:List[PlayerStats]) {
    val netState = NetState(
      "t" -> team,
      "tp" -> team_points,
      "ps" -> players_stats.map(_.netState)
    )
  }

  def teamStats(message:NetState):TeamStats = {
    TeamStats(
      team = message.value[Int]("t").get,
      team_points = message.value[Int]("tp").get,
      players_stats = message.value[List[NetState]]("ps").getOrElse(Nil).map(x => playerStats(x))
    )
  }

  case class GameStats(teams_stats:List[TeamStats], game_start_moment_sec:Option[Long], your_team:Int) {
    private val builder = NetState.newBuilder
    builder += ("ts" -> teams_stats.map(_.netState))
    game_start_moment_sec.foreach(gs => builder += ("gs" -> gs))
    builder += ("yt" -> your_team)
    val netState = builder.toState
  }

  def gameStats(message:NetState):GameStats = {
    GameStats(
      teams_stats = message.value[List[NetState]]("ts").getOrElse(Nil).map(x => teamStats(x)),
      game_start_moment_sec = message.value[Long]("gs"),
      your_team = message.value[Int]("yt").get
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
      if(!game_started) {
        GameStats(Nil, game_start_moment_sec = None, your_team)
      } else {
        val teams_stats = count.toList.map {
          case (team, points) =>
            val player_stats = players.values.flatten.filter(_.team == team).map(p => {
              PlayerStats(p.team, p.number_in_team, p.number, p.wins, p.deaths)
            }).toList
            TeamStats(team, points, player_stats)
        }
        GameStats(teams_stats, game_start_moment_sec = Some(game_start_moment_sec), your_team)
      }
    }
  }

  case class GameInfo(game_id:Int, team1_players:Int, team2_players:Int) {
    val players = team1_players+team2_players
    val netState = NetState(
      "gid" -> game_id,
      "t1p" -> team1_players,
      "t2p" -> team2_players
    )
  }

  case class JoinGame(game_id:Int, team:Option[Int]) {
    val netState = {
      val builder = NetState.newBuilder
      builder += ("gid" -> game_id)
      team.foreach(t => builder += ("t" -> t))
      builder.toState
    }
  }

  def joinGame(message:NetState):JoinGame = {
    JoinGame(
      game_id = message.value[Int]("gid").get,
      team =  message.value[Int]("t")
    )
  }

  def gameInfo(message:NetState):GameInfo = {
    GameInfo(message.value[Int]("gid").get,
             message.value[Int]("t1p").get,
             message.value[Int]("t2p").get)
  }

  def gamesList(message:NetState):List[GameInfo] = {
    message.value[List[NetState]]("gameslist").getOrElse(Nil).map(m => gameInfo(m))
  }

  case class ControlPoint(number:Int, var team:Option[Int], var control_start_time_sec:Long, area:List[Vec]) {
    val area_center = Vec(area.map(_.x).sum/area.length, area.map(_.y).sum/area.length)

    def controlPointColor(your_team:Int):ScageColor = {
      team match {
        case Some(cp_team) => if(cp_team != your_team) RED else GREEN
        case None => GRAY
      }
    }

    def netState = {
      val builder = NetState.newBuilder
      builder += ("n" -> number)
      if(team.nonEmpty) builder += ("t" -> team.get)
      builder += ("cst" -> control_start_time_sec)
      builder += ("a" -> area.map(p => NetState("x" -> p.x, "y" -> p.y)))
      builder.toState
    }

    def infoNetState = {
      val builder = NetState.newBuilder
      builder += ("n" -> number)
      if(team.nonEmpty) builder += ("t" -> team.get)
      builder += ("cst" -> control_start_time_sec)
      builder.toState
    }
  }

  def controlPoint(message:NetState):ControlPoint = {
    ControlPoint(
      number = message.value[Int]("n").get,
      team = message.value[Int]("t"),
      control_start_time_sec = message.value[Long]("cst").get,
      area = message.value[List[NetState]]("a").get.map(p => vec(p, "x", "y"))
    )
  }

  case class ControlPointInfo(number:Int, team:Option[Int], control_start_time:Long)

  def controlPointInfos(message:NetState):List[ControlPointInfo] = {
    message.value[List[NetState]]("cps_infos").getOrElse(Nil).map(x => {
      ControlPointInfo(
        number = x.value[Int]("n").get,
        team = x.value[Int]("t"),
        control_start_time = x.value[Long]("cst").get
      )
    })
  }

  case class GameMap(walls:List[Wall] = Nil, safe_zones:List[List[Vec]] = Nil, control_points:Map[Int, ControlPoint] = Map()) {
    /*private val walls_on_map = Array.fill(map_width/(human_size*2)+1, map_height/(human_size*2)+1)(ArrayBuffer[Wall]())
    walls.foreach {
      case w @ Wall(from ,to) =>
        bresenham(from ,to, -map_width/2, -map_height/2, human_size*2).foreach {
          case (x, y) =>
            /*println(w)
            println(x+":"+y)*/
            walls_on_map(x)(y) += w
        }
    }*/

    /*private def coordOnMap(v:Vec):(Int, Int) = {
      val x = ((v.x + map_width/2)/human_size/2).toInt
      val y = ((v.y + map_height/2)/human_size/2).toInt
      (x, y)
    }*/

    def isEmpty:Boolean = walls.isEmpty && safe_zones.isEmpty && control_points.isEmpty

    def isCoordCorrect(coord:Vec, body_size:Float):Boolean = {
      isCoordInsideMapBorders(coord) &&/* {
        val (x,y) = coordOnMap(coord)
        walls_on_map(x)(y).forall(w => !isCoordNearWall(coord, w, body_radius))
      }*/
      walls.forall(w => !isCoordNearWall(coord, w, body_size))
    }

    def isPathCorrect(from:Vec, to:Vec, body_radius:Float):Boolean = {
      walls.forall(w => !areLinesIntersect(from, to, w.from, w.to)) && isCoordCorrect(to, body_radius)
      /*bresenham(from ,to, -map_width/2, -map_height/2, human_size*2).forall {
        case (x, y) => walls_on_map(x)(y).forall(w => !areLinesIntersect(from, to, w.from, w.to))
      } && isCoordCorrect(to, body_radius)*/
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
        val team1_area = safe_zones(0)
        val team2_area = safe_zones(1)
        team match {
          case 1 => randomHumanCoordInsideArea(team1_area)
          case 2 => randomHumanCoordInsideArea(team2_area)
          case _ => randomHumanCoordInsideArea(team1_area)
        }
      }
    }

    def isCoordVisible(coord:Vec, from:Vec, pov:Vec):Boolean = {
      isCoordInsidePov(coord, from, pov) &&/* {
        bresenham(from ,coord, -map_width/2, -map_height/2, human_size*2).forall {
          case (x, y) => walls_on_map(x)(y).forall(w => !areLinesIntersect(from, coord, w.from, w.to))
        }
      }*/
      walls.forall(w => !areLinesIntersect(from, coord, w.from, w.to))
    }

    def isCoordVisibleOrAudible(coord:Vec, from:Vec, pov:Vec, is_moving:Boolean, audibility_radius:Float):Boolean = {
      is_moving && coord.dist2(from) <= audibility_radius * audibility_radius ||
      isCoordInsidePov(coord, from, pov) &&/* {
        bresenham(from ,coord, -map_width/2, -map_height/2, human_size*2).forall {
          case (x, y) => walls_on_map(x)(y).forall(w => !areLinesIntersect(from, coord, w.from, w.to))
        }
      }*/
      walls.forall(w => !areLinesIntersect(from, coord, w.from, w.to))
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

    val netState = NetState(
      "ws" -> walls.map(w => w.netState).toList,
      "szs" -> safe_zones.map(sz => sz.map(p => NetState("x" -> p.x, "y" -> p.y))),
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

  def gameMap(message:NetState):GameMap = {
    GameMap(
      walls = message.value[List[NetState]]("ws").getOrElse(Nil).map(m => wall(m)),
      safe_zones = message.value[List[List[NetState]]]("szs").getOrElse(Nil).map(m => m.map(p => vec(p, "x", "y"))),
      control_points = message.value[List[NetState]]("cps").getOrElse(Nil).map(m => {
        val cp = controlPoint(m)
        (cp.number, cp)
      }).toMap
    )
  }

  def isCoordNearWall(coord:Vec, wall:Wall, body_size:Float):Boolean = {
    val one = (wall.to - wall.from).rotateDeg(90).n*body_size/2 + wall.from
    val two = (wall.to - wall.from).rotateDeg(-90).n*body_size/2 + wall.from
    val three = (wall.from - wall.to).rotateDeg(90).n*body_size/2 + wall.to
    val four = (wall.from - wall.to).rotateDeg(-90).n*body_size/2 + wall.to
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
