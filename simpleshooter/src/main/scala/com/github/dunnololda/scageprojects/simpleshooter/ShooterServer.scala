package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, _}
import collection.mutable
import collection.mutable.ArrayBuffer
import com.github.dunnololda.cli.Cli

object ShooterServer extends ScageApp("Simple Shooter Server") with Cli {
  programDescription = s"Simple Shooter Server v$appVersion"
  commandLineArgsAndParse(
    ("p", "port", "port to bind server on. default: 10000", true, false),
    ("m", "map",  "map file to load. default: map.ss", true, false)
  )
  private val server = UdpNetServer(port = property("port", 10000), ping_timeout= 1000, check_timeout = 5000)

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

  private val map_name = property("map", "map.ss")

  private val players = mutable.HashMap[Long, Client]()
  private val bullets = ArrayBuffer[Bullet]()
  private val walls = loadMap(map_name)

  private def outsideCoord(coord:Vec):Vec = {
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

  // receive data
  action(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
        players(client_id) = Client(client_id, randomCoord(map_width, map_height, body_radius, walls), 100, 0, 0, visible = true)
      case NewUdpClientData(client_id, message) =>
        //println(message.toJsonString)
        if(message.contains("sendmap")) server.sendToClient(client_id, NetState("walls" -> walls.map(_.netState).toList))
        val ClientData(up, left, down, right, shoots) = clientData(message)
        val delta = Vec((if(left) -1 else 0) + (if(right) 1 else 0), (if(down) -1 else 0) + (if(up) 1 else 0)).n
        val new_coord = outsideCoord(players(client_id).coord + delta*speed)
        if(isCoordCorrect(new_coord, body_radius, walls)) {
          players(client_id).coord = new_coord
        }
        shoots.foreach(sh => {
          val dir = (sh - new_coord).n
          bullets += Bullet(dir, players(client_id), new_coord + dir*(body_radius+1), bullet_count)
        })
        //println(players(client_id).coord)
      case UdpClientDisconnected(client_id) =>
        players -= client_id
    }
  }

  // update state
  action(100) {
    bullets.foreach(b => {
      val new_coord = b.coord + b.dir*speed*bullet_speed_multiplier
      b.count -= 1
      if (!isPathCorrect(b.coord, new_coord, bullet_size, walls)) {
        b.count = 0
      } else b.coord = new_coord
      val damaged_players = players.values.filter(_.coord.dist2(b.coord) < 100)
      if (damaged_players.nonEmpty) {
        damaged_players.foreach(p => {
          p.health -= bullet_damage
          if (p.health <= 0) {
            p.deaths += 1
            p.coord = randomCoord(map_width, map_height, body_radius, walls)
            p.health = 100
            b.shooter.wins += 1
          }
        })
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
        val others = players.filterNot(_._1 == id).map(x => x._2.copy(visible = isCoordVisible(x._2.coord, client.coord, walls)).netState).toList
        if(others.nonEmpty) builder += ("others" -> others)
        val (your_bullets, other_bullets) = bullets.filter(b => isCoordVisible(b.coord, client.coord, walls)).partition(_.shooter.id == id)
        if (your_bullets.nonEmpty) builder += ("your_bullets" -> your_bullets.map(_.netState).toList)
        if (other_bullets.nonEmpty) builder += ("other_bullets" -> other_bullets.map(_.netState).toList)
        val data = NetState(builder:_*)
        server.sendToClient(id, data)
    }
  }
}
