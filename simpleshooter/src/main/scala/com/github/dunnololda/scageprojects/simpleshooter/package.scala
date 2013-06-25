package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState}
import collection.mutable.ArrayBuffer
import scala.collection.mutable

package object simpleshooter {
  val host = "localhost"
  val port = 10000
  val speed = 0.332f  // px / 10msec = 33.2 px / sec = 3.32 m/sec = 12 km/h
  val bullet_speed = 12f  // 12 px / 10msec = 1200 px / sec = 120 m/sec
  val bullet_count = 50  // 60 meters = 600 px = 0.5 sec = 50*10msec
  val bullet_damage = 100
  val map_width = 800
  val map_height = 600
  val body_radius = 10  // 10px is 1 meter
  val bullet_size = 3
  val pov_distance = 600
  val pov_angle = 50
  val fire_pace = 100 // 100 msec/bullet = 60000 msec / 600 bullet = 600 bullet/min -> AK
  val audibility_radius = 60

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

  case class TacticServerPlayer(
                          id:Long,
                          number:Int,
                          private var _coord:Vec,
                          private var _pov:Vec,
                          var health:Int,
                          var wins:Int,
                          var deaths:Int,
                          var visible:Boolean) {
    def coord = _coord
    def coord_=(new_coord:Vec) {
      _coord = new_coord
      _pov_area = povTriangle(_coord, _pov, pov_distance, pov_angle)
    }

    val ds = ArrayBuffer[Vec]()
    def pov = _pov
    def pov_=(new_pov:Vec) {
      _pov = new_pov
      _pov_area = povTriangle(_coord, _pov, pov_distance, pov_angle)
    }

    var last_bullet_shot = 0l

    private var _pov_area = povTriangle(_coord, _pov, pov_distance, pov_angle)
    def povArea = _pov_area

    def netState:NetState = NetState(
      "id" -> id,
      "n" -> number,
      "x"  -> _coord.x,
      "y"  -> _coord.y,
      "ds"  -> ds.toList.map(d => NetState("x" -> d.x, "y" -> d.y)),
      "px" -> _pov.x,
      "py" -> _pov.y,
      "pa"  -> _pov_area.toList.map(d => NetState("x" -> d.x, "y" -> d.y)),
      "hp" -> health,
      "w"  -> wins,
      "d"  -> deaths,
      "v" -> visible)
  }

  case class TacticClientPlayer(
     id:Long,
     number:Int,
     coord:Vec,
     destinations:List[Vec],
     pov:Vec,
     pov_area:List[Vec],
     health:Int,
     wins:Int,
     deaths:Int,
     visible:Boolean
  )

  def tacticClient(message:NetState):TacticClientPlayer = {
    TacticClientPlayer(
      id = message.value[Long]("id").get,
      number = message.value[Int]("n").get,
      coord = vec(message, "x", "y"),
      destinations = message.value[List[NetState]]("ds").getOrElse(Nil).map(m => vec(m, "x", "y")),
      pov = vec(message, "px", "py"),
      pov_area = message.value[List[NetState]]("pa").getOrElse(Nil).map(m => vec(m, "x", "y")),
      health = message.value[Int]("hp").get,
      wins = message.value[Int]("w").get,
      deaths = message.value[Int]("d").get,
      visible = message.value[Boolean]("v").get
    )
  }

  case class TacticClientData(player_num:Int, destination:Option[Vec], pov: Option[Vec], clear_destinations:Boolean)

  def tacticClientData(message:NetState):TacticClientData = {
    TacticClientData(
      player_num = message.value[Int]("pn").get,
      destination = message.value[NetState]("d").map(x => vec(x, "x", "y")),
      pov = message.value[NetState]("pov").map(x => vec(x, "x", "y")),
      clear_destinations = message.value[Boolean]("cleardest").getOrElse(false)
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

  case class TacticServerData(
                               yours:List[TacticClientPlayer],
                               others:List[TacticClientPlayer],
                               your_bullets:List[TacticClientBullet],
                               other_bullets:List[TacticClientBullet],
                               receive_moment:Long
  )

  def tacticServerData(message:NetState, receive_moment:Long):TacticServerData = {
    val yours = message.value[List[NetState]]("yours").getOrElse(Nil).map(m => tacticClient(m))
    val others = message.value[List[NetState]]("others").getOrElse(Nil).map(m => tacticClient(m))
    val your_bullets = message.value[List[NetState]]("your_bullets").getOrElse(Nil).map(m => tacticClientBullet(m))
    val other_bullets = message.value[List[NetState]]("other_bullets").getOrElse(Nil).map(m => tacticClientBullet(m))
    TacticServerData(yours, others, your_bullets, other_bullets, receive_moment)
  }

  case class Wall(from:Vec, to:Vec) {
    def netState = NetState("fromx" -> from.x, "fromy" -> from.y, "tox" -> to.x, "toy" -> to.y)
  }

  def wall(message:NetState):Wall = {
    Wall(Vec(message.value[Float]("fromx").get, message.value[Float]("fromy").get), Vec(message.value[Float]("tox").get, message.value[Float]("toy").get))
  }

  def serverWalls(message:NetState):List[Wall] = {
    message.value[List[NetState]]("walls").get.map(x => wall(x))
  }

  case class Bullet(dir:Vec, shooter:Client, var coord:Vec, var count:Int) {
    def netState = NetState("x" -> coord.x, "y" -> coord.y)
  }

  case class TacticBullet(id:Long, dir:Vec, shooter:TacticServerPlayer, var prev_coord:Vec, var coord:Vec, var count:Int) {
    def netState = NetState(
      "id" -> id,
      "pid" -> shooter.id,
      "pn" -> shooter.number,
      "x" -> coord.x,
      "y" -> coord.y
    )
  }

  case class TacticClientBullet(id:Long, player_id:Long, player_number:Int, coord:Vec)

  def tacticClientBullet(message:NetState):TacticClientBullet = {
    TacticClientBullet(
      id = message.value[Long]("id").get,
      player_id = message.value[Long]("pid").get,
      player_number = message.value[Int]("pn").get,
      coord = vec(message, "x", "y"))
  }

  case class TacticGame(game_id:Int,
                        players:mutable.HashMap[Long, List[TacticServerPlayer]] = mutable.HashMap[Long, List[TacticServerPlayer]](),
                        bullets:ArrayBuffer[TacticBullet] = ArrayBuffer[TacticBullet]())

  case class GameInfo(game_id:Int, players:Int) {
    def netState = NetState(
      "gid" -> game_id,
      "ps" -> players
    )
  }

  def gameInfo(message:NetState):GameInfo = {
    GameInfo(message.value[Int]("gid").get, message.value[Int]("ps").get)
  }

  def gamesList(message:NetState):List[GameInfo] = {
    message.value[List[NetState]]("gameslist").getOrElse(Nil).map(m => gameInfo(m))
  }

  def loadMap(map_name:String):List[Wall] = {
    def tryFloat(str:String):Boolean = {
      try {
        str.toFloat
        true
      } catch {
        case e:Exception => false
      }
    }

    try {
      (for {
        line <- io.Source.fromFile(map_name).getLines()
        if !line.startsWith("#")
        coords = line.split(" ")
        if coords.length == 4 && coords.forall(c => tryFloat(c))
      } yield Wall(Vec(coords(0).toFloat, coords(1).toFloat), Vec(coords(2).toFloat, coords(3).toFloat))).toList
    } catch {
      case e:Exception => Nil
    }
  }

  def isCoordNearWall(coord:Vec, wall:Wall, body_radius:Float):Boolean = {
    val one = (wall.to - wall.from).rotateDeg(135).n*body_radius + wall.from
    val two = (wall.to - wall.from).rotateDeg(-135).n*body_radius + wall.from
    val three = (wall.from - wall.to).rotateDeg(135).n*body_radius + wall.to
    val four = (wall.from - wall.to).rotateDeg(-135).n*body_radius + wall.to
    val area = List(one, two, three, four)
    coordOnArea(coord, area)
  }

  def isCoordCorrect(coord:Vec, body_radius:Float, walls:Seq[Wall]):Boolean = {
    walls.forall(w => !isCoordNearWall(coord, w, body_radius))
  }

  def randomCoord(width:Float, height:Float, near:Option[Vec], body_radius:Float, walls:Seq[Wall]):Vec = {
    near match {
      case Some(pos) =>
        val dir = Vec(math.random, math.random).n
        val coord = pos + dir*body_radius*3
        if (isCoordCorrect(coord, body_radius, walls)) coord
        else randomCoord(width, height, near, body_radius, walls)
      case None =>
        val coord = Vec(math.random*width, math.random*height)
        if (isCoordCorrect(coord, body_radius, walls)) coord
        else randomCoord(width, height, near, body_radius, walls)
    }
  }

  def isCoordVisible(coord:Vec, from:Vec, povArea:List[Vec], walls:Seq[Wall]):Boolean = {
    coordOnArea(coord, povArea) && walls.forall(w => {
      !areLinesIntersect(from, coord, w.from, w.to)
    })
  }

  def isCoordVisibleOrAudible(coord:Vec, from:Vec, povArea:List[Vec], is_moving:Boolean, audibility_radius:Float, walls:Seq[Wall]):Boolean = {
    is_moving && coord.dist2(from) <= audibility_radius * audibility_radius || coordOnArea(coord, povArea) && walls.forall(w => {
      !areLinesIntersect(from, coord, w.from, w.to)
    })
  }

  def isPathCorrect(from:Vec, to:Vec, body_radius:Float, walls:Seq[Wall]):Boolean = {
    walls.forall(w => !areLinesIntersect(from, to, w.from, w.to)) && isCoordCorrect(to, body_radius, walls)
  }

  def isBodyHit(from:Vec, to:Vec, body_position:Vec, body_radius:Float):Boolean = {
    val a = body_position + (to - from).rotateDeg(90)
    val b = body_position + (to - from).rotateDeg(-90)
    areLinesIntersect(from ,to, a, b)
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

  def outsideCoord(coord:Vec, width:Float, height:Float):Vec = {
    def checkC(c:Float, from:Float, to:Float):Float = {
      val dist = to - from
      if(c >= to) checkC(c - dist, from, to)
      else if(c < from) checkC(c + dist, from, to)
      else c
    }
    val x = checkC(coord.x, 0, width)
    val y = checkC(coord.y, 0, height)
    Vec(x, y)
  }

  def povTriangle(coord:Vec, pov:Vec, distance:Float, angle:Float):List[Vec] = {
    val axis = pov*distance
    val one = coord + axis.rotateDeg(angle)
    val two = coord + axis.rotateDeg(-angle)
    List(coord, one, two, coord)
  }

  def messageArea(message:Any, coord:Vec, printer:ScageMessage = ScageMessage):List[Vec] = {
    val Vec(w, h) = messageBounds(message)
    val Vec(x, y) = coord
    List(Vec(x-w/2, y+h/2), Vec(x+w/2, y+h/2), Vec(x+w/2, y-h/2), Vec(x-w/2, y-h/2))
  }

  def createMenuItems(menu_items:List[(String, Vec,  () => Any)], printer:ScageMessage = ScageMessage):List[(String, Vec, List[Vec], () => Any)] = {
    menu_items.map {
      case (title, coord, action) => (title, coord, messageArea(title, coord, printer), action)
    }
  }
}
