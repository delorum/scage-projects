package com.github.dunnololda.scageprojects.jetflight.online

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet._
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait FlightTrace extends Trace {
  def stateJson:JsValue
}

class FlightTracer extends CoordTracer[FlightTrace](solid_edges = false) {
  private val _planes = mutable.HashMap[Long, ServerPlane]()
  def plane(client_id:Long) = _planes(client_id)
  def addPlane(coord:Vec, plane:ServerPlane) {
    _planes += (plane.client_id -> plane)
    addTrace(coord, plane)
  }
  def removePlane(client_id:Long) {
    val plane = _planes(client_id)
    plane.stop()
    removeTraces(plane)
    _planes -= client_id        
  }
}

case class ServerDataCountUpdate(client_id:Long, wins:Int, loses:Int)

object NetFlight extends ScageApp(unit_name = "Net Flight") {
  implicit def vecWrites: Writes[Vec] = new Writes[Vec] {
    def writes(v:Vec) = JsObject(Seq(
      "x" -> JsNumber(BigDecimal(v.x)),
      "y" -> JsNumber(BigDecimal(v.y))
    ))
  }

  val tracer = new FlightTracer  

  private val count = mutable.HashMap[Long, (Int,  Int)]() // client -> (wins, deaths)
  def countData:JsArray = {
    JsArray((for {
      (client_id, (wins, loses)) <- count
    } yield Json.obj(
      "client_id" -> client_id,
      "wins" -> wins,
      "loses" -> loses
    )).toList)
  }
  private val count_updates = mutable.HashMap[Long, (Int,  Int)]() // client -> (wins, deaths)
  def countUpdatesData:JsArray = {
    JsArray((for {
      (client_id, (wins, loses)) <- count_updates
    } yield Json.obj(
      "client_id" -> client_id,
      "wins" -> wins,
      "loses" -> loses
    )).toList)
  }
  def fightResult(winer_id:Long, loser_id:Long) {
    val (winer_wins, winer_loses) = count(winer_id)
    count += (winer_id -> (winer_wins+1, winer_loses))
    
    val (loser_wins, loser_loses) = count(loser_id)
    count += (loser_id -> (loser_wins, loser_loses+1))
    
    count_updates ++= Map(winer_id -> count(winer_id), loser_id -> count(loser_id))
  }
  
  actionStaticPeriod(10000) {
    new HealthBar
  }
  
  val send_timeout = property("netflight.send_timeout", 50)

  val server = UdpNetServer(port = 15000)

  implicit val NetFlightClientData_reader = Json.reads[NetFlightClientData]
  
  actionStaticPeriod(10) {
    server.newEvent {
      case NewUdpConnection(new_client_id) =>
        new ServerPlane(new_client_id)
        count += (new_client_id ->(0, 0))
        count_updates += (new_client_id ->(0, 0))
        server.sendToClient(new_client_id, Json.obj(
          "your_plane_id" -> new_client_id,
          "count" -> countData
        ))
      case NewUdpClientData(client_id, received_data) =>
        received_data.validate[NetFlightClientData] match {
          case JsSuccess(netFlightClientData, _) =>
            netFlightClientData.keys.foreach {
              case ks =>
                val client_plane = tracer.plane(client_id)
                ks.foreach {
                  case 0 =>
                    client_plane.rotateLeft()
                  case 1 =>
                    client_plane.rotateRight()
                  case 2 =>
                    client_plane.reheat()
                  case 3 =>
                    client_plane.fire()
                  case _ =>
                }
            }
            if (netFlightClientData.goodbye.nonEmpty) {
              server.disconnectClient(client_id)
            }
          case JsError(error) =>
            println(s"[server] received_data.validate[NetFlightClientData] error: $error")

        }
      case UdpClientDisconnected(client_id) =>
        tracer.removePlane(client_id)
        count -= client_id
        server.sendToAll(Json.obj("leaver" -> client_id))
    }
  }

  actionStaticPeriod(send_timeout) {
    if(server.clientIds.nonEmpty) {
      val fields = ArrayBuffer[(String, JsValue)]("planes_rockets" -> JsArray(tracer.tracesList.map(_.stateJson)))
      val serverData = JsObject(fields)
      if (count_updates.nonEmpty) {
        fields += ("count" -> countUpdatesData)
        count_updates.clear()
      }
      server.sendToAll(serverData)
    }
  }
}

import NetFlight._

object ServerObjectType extends Enumeration {
  type ServerObjectType = Value
  val Plane, Rocket, Word, Health, Ender = Value 
}

case class ServerData(planes_rockets:Option[List[JsValue]], count:Option[List[ServerDataCountUpdate]], your_plane_id:Option[Long], leaver:Option[Long])

case class ServerDataPlane(client_id:Long, coord:Vec, health:Int, rotation:Float, speed:Float, object_type:ServerObjectType.Value)

class ServerPlane(val client_id: Long) extends FlyingObject with FlightTrace {
  tracer.addPlane(tracer.randomCoord(), this)

  _rotation = (math.random * 20 - 10).toFloat
  
  protected var _health = 100
  def health = _health

  protected var plane_side = 1

  def state = State("client_id" -> client_id,
                    "coord"     -> location,
                    "health"    -> _health,
                    "rotation"  -> _rotation,
                    "speed"     -> _speed,
                    "object_type"      -> ServerObjectType.Plane.toString)
  
  def stateJson = Json.obj(
    "client_id" -> client_id,
    "coord"     -> location,
    "health"    -> _health,
    "rotation"  -> _rotation,
    "speed"     -> _speed,
    "object_type"      -> Json.toJson(ServerObjectType.Plane)(EnumUtils.enumWrites)
  )
  
  def changeState(changer: Trace, s: State) {
    s.neededKeys {
      case ("damage", damage: Int) => 
        _health -= damage
        if(_health <= 0) changer.changeState(this, State("killer"))
      case ("health", plus_health:Int) => _health += plus_health
    }
  }

  private val action_id = actionStaticPeriod(10) {
    if (_health <= 0) {
      _health = 100
      tracer.updateLocation(this, tracer.randomCoord())
      _rotation = (math.random * 20 - 10).toFloat
    }

    tracer.updateLocation(this, location + step)
    if (_speed > 5) _speed -= 0.1f
  }
  
  def fire() {
    new ServerRocket(this, tracer.outsideCoord(location + step.n.rotate(math.Pi/2 * plane_side)*10), speed, rotation)
    plane_side *= -1
  }

  def stop() {
    delActions(action_id)
  }
}

case class ServerDataRocket(client_id:Long, rocket_id:Int, coord:Vec, fuel:Int, rotation:Float, speed:Float, object_type:ServerObjectType.Value)

class ServerRocket(shooter:ServerPlane, init_coord:Vec, init_speed:Float, init_rotation:Float) extends FlyingObject with FlightTrace {
  tracer.addTrace(init_coord, this)
  
  private var _fuel = 60
  _rotation = init_rotation
  _speed = init_speed + 10

  def state = State("client_id" -> shooter.client_id,
                    "rocket_id" -> id,
                    "coord" -> location,
                    "fuel" -> _fuel,
                    "rotation" -> _rotation,
                    "speed" -> _speed,
                    "object_type" -> ServerObjectType.Rocket.toString)

  def stateJson = Json.obj(
    "client_id" -> shooter.client_id,
    "rocket_id" -> id,
    "coord"     -> location,
    "fuel"    -> _fuel,
    "rotation" -> _rotation,
    "speed" -> _speed,
    "object_type" -> Json.toJson(ServerObjectType.Rocket)(EnumUtils.enumWrites)
  )

  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("killer", true) =>
        changer.state.value[Long]("client_id").foreach {
          case x => fightResult(shooter.client_id, x)
        }
    }
  }

  actionStaticPeriod(10) {
    if(_fuel > 0) {
      tracer.updateLocation(this, location + step)

      val target_planes = tracer.tracesNearCoord(location, -1 to 1,
        condition = other_trace =>
          other_trace.id != id &&
          other_trace.id != shooter.id &&
          other_trace.state.valueOrDefault("object_type", "unknown type") == ServerObjectType.Plane.toString &&
          other_trace.state.valueOrDefault("health", 0) > 0 &&
          (other_trace.location dist location) < (10+30))
      if(target_planes.nonEmpty) {
        val damage = (math.random*20).toInt
        target_planes.foreach(_.changeState(this, State("damage" -> damage)))
        new ServerFlyingWord(shooter.client_id, damage, location, step)
        _fuel = 0
      } else _fuel -= 1
    } else {
      deleteSelf()
      tracer.removeTraces(this)
    }
  }
}

case class ServerDataWord(client_id:Long, word_id:Int, coord:Vec, dir:Vec, message:String, object_type:ServerObjectType.Value)

class ServerFlyingWord(client_id:Long, message:Any, init_coord:Vec, direction:Vec) extends FlyingObject with FlightTrace {
  tracer.addTrace(init_coord, this)
  
  def state = State("client_id" -> client_id,
                    "word_id" -> id,
                    "coord" -> location,
                    "dir" -> _dir,
                    "message" -> message.toString,
                    "object_type" -> ServerObjectType.Word.toString)

  def stateJson = Json.obj(
    "client_id" -> client_id,
    "word_id" -> id,
    "coord" -> location,
    "dir" -> _dir,
    "message" -> message.toString,
    "object_type" ->  Json.toJson(ServerObjectType.Word)(EnumUtils.enumWrites)
  )

  def changeState(changer:Trace, s:State) {}

  private var _lifetime = 10

  private val _dir = direction.n
  private var _coord = init_coord

  actionStaticPeriod(10) {
    if(_lifetime > 0) {
      _coord += _dir
      _lifetime -= 1
    } else {
      deleteSelf()
      tracer.removeTraces(this)
    }
  }
}

case class ServerDataHealthBar(health_id:Int, coord:Vec, object_type:ServerObjectType.Value)

class HealthBar extends FlightTrace {
  tracer.addTrace(tracer.randomCoord(), this)
  
  def state = State("health_id" -> id,
                    "coord" -> location,
                    "object_type" -> ServerObjectType.Health.toString)

  def stateJson = Json.obj(
    "health_id" -> id,
    "coord" -> location,
    "object_type" -> Json.toJson(ServerObjectType.Health)(EnumUtils.enumWrites)
  )

  def changeState(changer:Trace, s:State) {}

  private var _lifetime = 3000; // 30 seconds
  
  actionStaticPeriod(10) {
    if(_lifetime > 0) {
      val planes_near = tracer.tracesInPoint(tracer.point(location), condition = {trace =>
        trace.state.valueOrDefault("object_type", "unknown typ") == ServerObjectType.Plane.toString &&
        (trace.location dist location) < (10+30)})
      if(planes_near.nonEmpty) {
        planes_near.head.changeState(this, State("health" -> 20))
        _lifetime = 0
      } else _lifetime -= 1
    } else {
      deleteSelf()
      tracer.removeTraces(this)
    }
  }
}
