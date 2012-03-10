package net.scage.tutorial.netflight

import net.scage.ScageLib._
import net.scage.support.net.NetServer
import net.scage.support.{State, Vec}
import net.scage.support.tracer3.{Trace, CoordTracer}
import collection.mutable.{HashMap, ArrayBuffer}
import net.scage.ScageApp

class FlightTracer extends CoordTracer[Trace](solid_edges = false) {
  private val _planes = HashMap[Int, ServerPlane]()
  def plane(client_id:Int) = _planes(client_id)
  def addPlane(coord:Vec, plane:ServerPlane) {
    _planes += (plane.client_id -> plane)
    addTrace(coord, plane)
  }
  def removePlane(client_id:Int) {
    val plane = _planes(client_id)
    plane.stop()
    removeTraces(plane)
    _planes -= client_id        
  }
}

object NetFlight extends ScageApp(unit_name = "Net Flight") {
  val tracer = new FlightTracer  

  private val count = HashMap[Int, (Int,  Int)]() // client -> (wins, deaths)
  def countData:List[State] = {
    (for {
      (client_id, (wins, loses)) <- count
    } yield State("client_id" -> client_id, 
                  "wins" -> wins, 
                  "loses" -> loses)).toList
  }
  private val count_updates = HashMap[Int, (Int,  Int)]() // client -> (wins, deaths)
  def countUpdatesData:List[State] = {
    (for {
      (client_id, (wins, loses)) <- count_updates
    } yield State("client_id" -> client_id, 
                  "wins" -> wins, 
                  "loses" -> loses)).toList
  }
  def fightResult(winer_id:Int, loser_id:Int) {
    val (winer_wins, winer_loses) = count(winer_id)
    count += (winer_id -> (winer_wins+1, winer_loses))
    
    val (loser_wins, loser_loses) = count(loser_id)
    count += (loser_id -> (loser_wins, loser_loses+1))
    
    count_updates ++= Map(winer_id -> count(winer_id), loser_id -> count(loser_id))
  }
  
  action(10000) {
    new HealthBar
  }
  
  val send_timeout = property("netflight.send_timeout", 50)

  NetServer.startServer(
    onClientAccepted = {
      client =>
        new ServerPlane(client.id)
        count += (client.id -> (0,0))
        count_updates += (client.id -> (0,0))
        client.send(State("your_plane_id" -> client.id, "count" -> countData))
    },
    onClientDataReceived = {
      (client, data) =>
        data.neededKeys {
          case ("keys", keys:List[Float]) =>
            val client_plane = tracer.plane(client.id)
            keys.foreach(key => key match {
              case 0 => client_plane.rotateLeft()
              case 1 => client_plane.rotateRight()
              case 2 => client_plane.reheat()
              case 3 => client_plane.fire()
              case _ =>
            })
          case ("goodbye", true) =>
            NetServer.disconnectClient(client)
          /*case ("left", true) => client_plane.rotateLeft()
          case ("right", true) => client_plane.rotateRight()
          case ("up", true) => client_plane.reheat()*/
          //case ("lctrl", true) => client_plane.fire()
        }
        /*val plane_data = State("plane" -> client_plane.toState)
        NetServer.sendToAll(plane_data)*/
    },
    onClientDisconnected = {
      client =>
        tracer.removePlane(client.id)
        count -= client.id
        NetServer.sendToAll(State("leaver" -> client.id))
    }
  )
  
  action(send_timeout) {
    val data = State("planes_rockets" -> tracer.tracesList.map(_.state).toList)
    if(!count_updates.isEmpty) {
      data ++= State("count" -> countUpdatesData)
      count_updates.clear()
    }
    NetServer.sendToAll(data)
  }
}

import NetFlight._

class ServerPlane(val client_id: Int) extends FlyingObject with Trace {
  tracer.addPlane(tracer.randomCoord(), this)

  _rotation = (math.random * 20 - 10).toFloat
  
  protected var _health = 100
  def health = _health

  protected var plane_side = 1

  def state = State("client_id" -> client_id,
                    "coord" -> location,
                    "health" -> _health,
                    "rotation" -> _rotation,
                    "speed" -> _speed,
                    "type" -> "plane")
  
  def changeState(changer: Trace, s: State) {
    s.neededKeys {
      case ("damage", damage: Int) => 
        _health -= damage
        if(_health <= 0) changer.changeState(this, State("killer"))
      case ("health", plus_health:Int) => _health += plus_health
    }
  }

  private val action_id = action(10) {
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

class ServerRocket(shooter:ServerPlane, init_coord:Vec, init_speed:Float, init_rotation:Float) extends FlyingObject with Trace {
  tracer.addTrace(init_coord, this)
  
  private var _fuel = 60
  _rotation = init_rotation
  _speed = init_speed + 10

  def state = State("client_id" -> shooter.client_id,
                    "coord" -> location,
                    "fuel" -> _fuel, 
                    "rocket_id" -> id,
                    "rotation" -> _rotation,
                    "speed" -> _speed,
                    "type" -> "rocket")
  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("killer", true) =>
        fightResult(shooter.client_id, changer.state.value[Int]("client_id"))
    }
  }

  action(10) {
    if(_fuel > 0) {
      tracer.updateLocation(this, location + step)

      val target_planes = tracer.tracesNearCoord(location, -1 to 1,
        condition = other_trace =>
          other_trace.id != id &&
          other_trace.id != shooter.id &&
          other_trace.state.valueOrDefault("type", "unknown type") == "plane" &&
          other_trace.state.valueOrDefault("health", 0) > 0 &&
          (other_trace.location dist location) < (10+30))
      if(!target_planes.isEmpty) {
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

class ServerFlyingWord(client_id:Int, message:Any, init_coord:Vec, direction:Vec) extends FlyingObject with Trace {
  tracer.addTrace(init_coord, this)

  def state = State("client_id" -> client_id,
                    "coord" -> location,
                    "dir" -> _dir,
                    "message" -> message.toString,
                    "type" -> "word",
                    "word_id" -> id)
  def changeState(changer:Trace, s:State) {}

  private var _lifetime = 10;

  private val _dir = direction.n
  private var _coord = init_coord

  action(10) {
    if(_lifetime > 0) {
      _coord += _dir
      _lifetime -= 1
    } else {
      deleteSelf()
      tracer.removeTraces(this)
    }
  }
}

class HealthBar extends Trace {
  tracer.addTrace(tracer.randomCoord(), this)
  
  def state = State("coord" -> location,
                    "health_id" -> id,
                    "type" -> "health")
  def changeState(changer:Trace, s:State) {}

  private var _lifetime = 3000; // 30 seconds
  
  action(10) {
    if(_lifetime > 0) {
      val planes_near = tracer.tracesInPoint(tracer.point(location), condition = {trace =>
        trace.state.valueOrDefault("type", "unknown typ") == "plane" &&
        (trace.location dist location) < (10+30)})
      if(!planes_near.isEmpty) {
        planes_near.head.changeState(this, State("health" -> 20))
        _lifetime = 0
      } else _lifetime -= 1
    } else {
      deleteSelf()
      tracer.removeTraces(this)
    }
  }
}

class Ender(object_type:String,  object_id:Int) extends Trace {
  tracer.addTrace(tracer.randomCoord(), this)

  def state = State("object_id" -> object_id,
                    "object_type" -> object_type,
                    "type" -> "ender")
  def changeState(changer:Trace, s:State) {}
}
