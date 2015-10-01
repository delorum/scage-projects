package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet._
import play.api.libs.json._
import collection.mutable
import collection.mutable.ArrayBuffer
import com.github.dunnololda.cli.Cli

case class MapData(ws:List[Wall], szs:List[List[Vec]], cps:List[ControlPoint])
case class ShooterServerData(map:Option[MapData], you:Option[Client], others:Option[List[Client]], your_bulltes:Option[List[Vec]], other_bullets:Option[List[Vec]])

object ShooterServer extends ScageApp("Simple Shooter Server") with Cli {
  programDescription = s"Simple Shooter Server v$appVersion"
  commandLineArgsAndParse(
    ("p", "port", "port to bind server on. default: 10000", true, false),
    ("m", "map",  "map file to load. default: map.ss", true, false)
  )
  private val server = UdpNetServer(port = property("port", 10000), ping_timeout= 1000, check_if_offline_timeout = 5000)

  private val map_name = property("map", "map.ss")

  private val players = mutable.HashMap[Long, Client]()
  private val bullets = ArrayBuffer[Bullet]()
  private val map = loadMap(map_name)

  implicit val VecJson_reader = {
    case class VecJson(x:Float, y:Float)
    import play.api.libs.functional.syntax._
    (
      (__ \ "x").read[Float] and
      (__ \ "y").read[Float]
    )(VecJson.apply _).map(z => Vec(z.x, z.y))
  }
  implicit val ShooterClientData_reader = Json.reads[ShooterClientData]

  // receive data
  actionStaticPeriod(10) {
    server.newEvent {
      case NewUdpConnection(client_id) =>
        players(client_id) = Client(client_id, map.randomHumanCoord, 100, 0, 0, visible = true)
        server.sendToClient(client_id, Json.obj("map" -> map.netState))
      case NewUdpClientData(client_id, received_data) =>
        received_data.validate[ShooterClientData] match {
          case JsSuccess(data, _) =>
            if(data.sendmap.nonEmpty) server.sendToClient(client_id, Json.obj("map" -> map.netState))
            val delta = Vec(
              (if(data.left.nonEmpty) -1 else 0) + (if(data.right.nonEmpty) 1 else 0),
              (if(data.down.nonEmpty) -1 else 0) + (if(data.up.nonEmpty) 1 else 0)).n
            val new_coord = outsideCoord(players(client_id).coord + delta*human_speed)
            if(map.isCoordCorrect(new_coord, human_size)) {
              players(client_id).coord = new_coord
            }
            data.shoots.foreach(shl => {shl.foreach { sh =>
              val dir = (sh - new_coord).n
              bullets += Bullet(dir, players(client_id), new_coord + dir * (human_size / 2 + 1))
            }})
          case JsError(error) =>
            println(s"[server] failed to parse client data $received_data: $error")
        }
      case UdpClientDisconnected(client_id) =>
        players -= client_id
    }
  }

  // update state
  actionStaticPeriod(100) {
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
  actionStaticPeriod(100) {
    players.foreach {
      case (id, client) =>
        val fields = ArrayBuffer[(String, JsValue)]()
        fields += ("you" -> client.netState)
        val others = players.filterNot(_._1 == id).map(x => x._2.copy(visible = isCoordVisible(x._2.coord, client.coord, map.walls)).netState).toList
        if(others.nonEmpty) fields += ("others" -> JsArray(others))
        val (your_bullets, other_bullets) = bullets.filter(b => isCoordVisible(b.coord, client.coord, map.walls)).partition(_.shooter.id == id)
        if (your_bullets.nonEmpty) fields += ("your_bullets" -> JsArray(your_bullets.map(_.netState)))
        if (other_bullets.nonEmpty) fields += ("other_bullets" -> JsArray(other_bullets.map(_.netState)))
        val data = JsObject(fields)
        server.sendToClient(id, data)
    }
  }
}
