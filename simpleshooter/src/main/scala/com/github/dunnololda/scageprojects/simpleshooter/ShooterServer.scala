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

  private val map_name = property("map", "map.ss")

  private val players = mutable.HashMap[Long, Client]()
  private val bullets = ArrayBuffer[Bullet]()
  private val map = loadMap(map_name)

  // receive data
  action(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
        players(client_id) = Client(client_id, map.randomHumanCoord, 100, 0, 0, visible = true)
        server.sendToClient(client_id, NetState("map" -> map.netState))
      case NewUdpClientData(client_id, message) =>
        //println(message.toJsonString)
        if(message.contains("sendmap")) server.sendToClient(client_id, NetState("map" -> map.netState))
        val ClientData(up, left, down, right, shoots) = clientData(message)
        val delta = Vec((if(left) -1 else 0) + (if(right) 1 else 0), (if(down) -1 else 0) + (if(up) 1 else 0)).n
        val new_coord = outsideCoord(players(client_id).coord + delta*human_speed)
        if(map.isCoordCorrect(new_coord, human_size)) {
          players(client_id).coord = new_coord
        }
        shoots.foreach(sh => {
          val dir = (sh - new_coord).n
          bullets += Bullet(dir, players(client_id), new_coord + dir*(human_size/2+1))
        })
        //println(players(client_id).coord)
      case UdpClientDisconnected(client_id) =>
        players -= client_id
    }
  }

  // update state
  action(100) {
    bullets.foreach(b => {
      val new_coord = b.coord + b.dir*bullet_speed
      b.count -= 1
      if (!map.isPathCorrect(b.coord, new_coord, bullet_size)) {
        b.count = 0
      } else b.coord = new_coord
      val damaged_players = players.values.filter(_.coord.dist2(b.coord) < 100)
      if (damaged_players.nonEmpty) {
        damaged_players.foreach(p => {
          p.health -= bullet_damage
          if (p.health <= 0) {
            p.deaths += 1
            p.coord = map.randomHumanCoord
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
        val others = players.filterNot(_._1 == id).map(x => x._2.copy(visible = isCoordVisible(x._2.coord, client.coord, map.walls)).netState).toList
        if(others.nonEmpty) builder += ("others" -> others)
        val (your_bullets, other_bullets) = bullets.filter(b => isCoordVisible(b.coord, client.coord, map.walls)).partition(_.shooter.id == id)
        if (your_bullets.nonEmpty) builder += ("your_bullets" -> your_bullets.map(_.netState).toList)
        if (other_bullets.nonEmpty) builder += ("other_bullets" -> other_bullets.map(_.netState).toList)
        val data = NetState(builder:_*)
        server.sendToClient(id, data)
    }
  }
}
