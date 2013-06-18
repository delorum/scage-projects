package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, ClientDisconnected, NewMessage, NewClient, NetServer}
import collection.mutable
import collection.mutable.ArrayBuffer

case class Client(id:Long, var coord:Vec, var health:Int) {
  def netState:NetState = NetState("id" -> id, "x" -> coord.x, "y" -> coord.y, "hp" -> health)
}
case class ClientData(up:Boolean, left:Boolean, down:Boolean, right:Boolean, shoots:List[Vec])

case class Wall(from:Vec, to:Vec) {
  def netState = NetState("fromx" -> from.x, "fromy" -> from.y, "tox" -> to.x, "toy" -> to.y)
}

case class Bullet(dir:Vec, shooter:Long, var coord:Vec, var count:Int) {
  def netState = NetState("shooter" -> shooter, "x" -> coord.x, "y" -> coord.y)
}

object ShooterServer extends ScageApp("Simple Shooter Server") {
  private val server = NetServer(10000)

  private val speed = 9f

  def vec(message:NetState):Vec = {
    Vec(message.value[Float]("x").get, message.value[Float]("y").get)
  }

  private def clientData(message:NetState):ClientData = {
    ClientData(
      up = message.valueOrDefault("up", false),
      left = message.valueOrDefault("left", false),
      down = message.valueOrDefault("down", false),
      right = message.valueOrDefault("right", false),
      shoots = message.valueOrDefault[List[NetState]]("shoots", Nil).map(x => vec(x))

    )
  }

  private def randomPos:Vec = Vec(math.random*640, math.random*480)

  private val players = mutable.HashMap[Long, Client]()
  private val bullets = ArrayBuffer[Bullet]()

  private val walls = List(Wall(Vec(320, 50), Vec(320, 240-40)), Wall(Vec(320, 240+40), Vec(320, 480-50)))

  private def isCoordNearWall(coord:Vec, wall:Wall, proximity:Float):Boolean = {
    val one = (wall.to - wall.from).rotateDeg(135).n*proximity + wall.from
    val two = (wall.to - wall.from).rotateDeg(-135).n*proximity + wall.from
    val three = (wall.from - wall.to).rotateDeg(135).n*proximity + wall.to
    val four = (wall.from - wall.to).rotateDeg(-135).n*proximity + wall.to
    val area = List(one, two, three, four)
    coordOnArea(coord, area)
  }

  // receive data
  action(10) {
    server.newEvent {
      case NewClient(client_id) =>
        players(client_id) = Client(client_id, randomPos, 100)
        server.sendToClient(client_id, NetState("walls" -> walls.map(_.netState).toList))
      case NewMessage(client_id, message) =>
        println(message.toJsonString)
        val ClientData(up, left, down, right, shoots) = clientData(message)
        val delta = Vec((if(left) -1 else 0) + (if(right) 1 else 0), (if(down) -1 else 0) + (if(up) 1 else 0)).n
        val new_coord = players(client_id).coord + delta*speed
        if(walls.forall(w => !isCoordNearWall(new_coord, w, 10))) {
          players(client_id).coord = new_coord
        }
        shoots.foreach(sh => {
          val dir = (sh - new_coord).n
          bullets += Bullet(dir, client_id, new_coord + dir*11, 30)
        })
        //println(players(client_id).coord)
      case ClientDisconnected(client_id) =>
        players -= client_id
    }
  }

  // update state
  action(100) {
    bullets.foreach(b => {
      val new_coord = b.coord + b.dir*speed*1.2f
      b.count -= 1
      if (walls.exists(w => areLinesIntersect(b.coord, new_coord, w.from, w.to))) {
        b.count = 0
      } else b.coord = new_coord
      val damaged_players = players.filter(p => p._2.coord.dist2(b.coord) < 100)
      if (damaged_players.nonEmpty) {
        damaged_players.foreach(_._2.health -= 10)
        b.count = 0
      }
    })
    bullets --= bullets.filter(b => b.count <= 0)
  }

  // send data
  action(100) {
    players.foreach {
      case (id, client) =>
        val builder = ArrayBuffer[(String, Any)]()
        builder += ("you" -> client.netState)
        val others = players.filterNot(_._1 == id).filter(x => {
          walls.forall(w => {
            !areLinesIntersect(client.coord, x._2.coord, w.from, w.to)
          })
        }).map(x => x._2.netState).toList
        if(others.nonEmpty) builder += ("others" -> others)
        val (your_bullets, other_bullets) = bullets.filter(b => {
          walls.forall(w => {
            !areLinesIntersect(client.coord, b.coord, w.from, w.to)
          })
        }).partition(b => b.shooter == id)
        if (your_bullets.nonEmpty) builder += ("your_bullets" -> your_bullets.map(_.netState).toList)
        if (other_bullets.nonEmpty) builder += ("other_bullets" -> other_bullets.map(_.netState).toList)
        val data = NetState(builder:_*)
        server.sendToClient(id, data)
    }
  }
}
