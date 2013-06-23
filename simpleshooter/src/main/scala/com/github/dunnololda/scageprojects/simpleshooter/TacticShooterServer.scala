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
    ("p", "port", "port to bind server on. default: 10000", true, false),
    ("m", "map",  "map file to load. default: map.ss", true, false)
  )
  private val server = UdpNetServer(port = property("port", 10000), ping_timeout= 1000, check_timeout = 5000)

  private val map_name = property("map", "map.ss")

  private val players = mutable.HashMap[Long, TacticServerPlayer]()
  private val bullets = ArrayBuffer[TacticBullet]()
  private val walls = loadMap(map_name)

  // receive data
  action(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
        val coord = randomCoord(map_width, map_height, body_radius, walls)
        players(client_id) = TacticServerPlayer(
          client_id,
          coord,
          _pov = Vec(0, 1),
          health = 100,
          wins = 0,
          deaths = 0,
          visible = true)
        server.sendToClient(client_id, NetState("walls" -> walls.map(_.netState).toList))
      case NewUdpClientData(client_id, message) =>
        val player = players(client_id)
        if(message.contains("sendmap")) server.sendToClient(client_id, NetState("walls" -> walls.map(_.netState).toList))
        if(message.contains("cleardest")) player.ds.clear()
        val TacticClientData(destination, pov) = tacticClientData(message)
        destination.foreach(d => player.ds += d)
        pov.foreach(p => player.pov = (p - player.coord).n)
      case UdpClientDisconnected(client_id) =>
        players -= client_id
    }
  }

  // update state
  action(10) {
    players.values.foreach(p => {
      p.ds.headOption.foreach(d => {
        if(d.dist2(p.coord) > speed*speed) {
          val new_coord = outsideCoord(p.coord + (d - p.coord).n*speed, map_width, map_height)
          if(isCoordCorrect(new_coord, body_radius, walls)) {
            p.coord = new_coord
          } else p.ds.clear()
        } else p.ds.remove(0)
      })
      players
        .values
        .filter(op => op.id != p.id &&
                isCoordVisible(op.coord, p.coord, p.povArea, walls) &&
                System.currentTimeMillis() - op.last_bullet_shot > fire_pace)
        .headOption.foreach(x => {
        val dir = (x.coord - p.coord).n
        bullets += TacticBullet(nextId, dir, p, p.coord + dir*(body_radius+1), bullet_count)
        x.last_bullet_shot = System.currentTimeMillis()
      })
    })
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
            p.pov = Vec(0, 1)
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
  action(50) {
    players.foreach {
      case (id, client) =>
        val builder = ArrayBuffer[(String, Any)]()
        builder += ("you" -> client.netState)
        val others = players.filterNot(_._1 == id).map(x => x._2.copy(visible = isCoordVisibleOrAudible(x._2.coord, client.coord, client.povArea, x._2.ds.nonEmpty, audibility_radius, walls)).netState).toList
        if(others.nonEmpty) builder += ("others" -> others)
        val (your_bullets, other_bullets) = bullets.filter(b => isCoordVisibleOrAudible(b.coord, client.coord, client.povArea, is_moving = true, audibility_radius, walls)).partition(_.shooter.id == id)
        if (your_bullets.nonEmpty) builder += ("your_bullets" -> your_bullets.map(_.netState).toList)
        if (other_bullets.nonEmpty) builder += ("other_bullets" -> other_bullets.map(_.netState).toList)
        val data = NetState(builder:_*)
        server.sendToClient(id, data)
    }
  }
}
