package net.scage.tutorial.netflight

import net.scage.ScageLib._
import net.scage.support.net.NetServer
import net.scage.support.{State, Vec}
import net.scage.support.tracer3.{Trace, CoordTracer}
import collection.mutable.{HashMap, ArrayBuffer}
import net.scage.{ScageScreenApp, ScageApp}

object NetFlight extends ScageApp(unit_name = "Net Flight") {
  val tracer = CoordTracer(solid_edges = false)
  private val planes = HashMap[Int, Plane]()
  val send_timeout = property("netflight.send_timeout", 50)

  NetServer.startServer(
    onClientAccepted = {
      client =>
        planes += (client.id -> new Plane(client.id))
        client.send(State("your_plane_id" -> client.id))
    },
    onClientDataReceived = {
      (client, data) =>
        val client_plane = planes(client.id)
        data.neededKeys {
          case ("keys", keys:List[Float]) =>
            keys.foreach(key => key match {
              case 0 => client_plane.rotateLeft()
              case 1 => client_plane.rotateRight()
              case 2 => client_plane.reheat()
              case _ =>
            })
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
        planes(client.id).stop()
        planes -= client.id
    }
  )
  
  action(send_timeout) {
    val planes_data = State("planes" -> planes.values.map(_.toState).toList)
    NetServer.sendToAll(planes_data)
  }
}

import NetFlight._

class Plane(val client_id: Int) extends FlyingObject {
  def toState = State("coord" -> trace.location,
                      "health" -> _health,
                      "id" -> client_id,
                      "rotation" -> _rotation,
                      "speed" -> _speed)
  
  protected var _health = 100
  def health = _health

  protected var plane_side = 1

  private val trace = tracer.addTrace(tracer.randomCoord(), new Trace {
    def state = State("type" -> "plane", "health" -> _health)

    def changeState(changer: Trace, s: State) {
      s.neededKeys {
        case ("damage", damage: Int) => _health -= damage
      }
    }
  })

  private val action_id = action(10) {
    if (_health <= 0) {
      _health = 100
      tracer.updateLocation(trace, tracer.randomCoord())
      _rotation = (math.random * 20 - 10).toFloat
    }

    tracer.updateLocation(trace, trace.location + step)
    if (_speed > 5) _speed -= 0.1f
  }

  def rotateLeft() {
    _rotation += 0.2f*_speed
  }

  def rotateRight() {
    _rotation -= 0.2f*_speed
  }

  def reheat() {
    if(_speed < 15) _speed += 0.5f
  }
  
  /*def fire() {
    new Rocket(trace.id, tracer.outsideCoord(trace.location + step.n.rotate(math.Pi/2 * plane_side)*10), speed, rotation)
    plane_side *= -1
  }*/

  def stop() {
    delActions(action_id)
    tracer.removeTraces(trace)
  }
}

/*class Rocket(shooter_id:Int, init_coord:Vec, init_speed:Float, init_rotation:Float) extends FlyingObject {
  private var fuel = 60
  rotation = init_rotation
  speed = init_speed + 10

  val trace = tracer.addTrace(init_coord, new Trace {
    def state = State("type" -> "rocket")
    def changeState(changer:Trace, s:State) {}
  })

  action {
    if(fuel > 0) {
      tracer.updateLocation(trace, trace.location + step)

      val target_planes = tracer.tracesNearCoord(trace.location, -1 to 1,
        condition = other_trace =>
          other_trace.id != trace.id &&
          other_trace.id != shooter_id &&
          other_trace.state.valueOrDefault("type", "unknown type") == "plane" &&
          other_trace.state.valueOrDefault("health", 0) > 0 &&
          (other_trace.location dist trace.location) < (10+30))
      if(!target_planes.isEmpty) {
        val damage = (math.random*20).toInt
        target_planes.foreach(_.changeState(trace, State("damage" -> damage)))
        /*lazy val flying_word_color = shooter_id match {
          case OurPlane.trace.id => RED
          case EnemyPlane.trace.id => YELLOW
          case _ => RED
        }
        new FlyingWord(damage, flying_word_color, trace.location, step)*/
        fuel = 0
      }

      fuel -= 1
      next_frame += 1
      if(next_frame >= ROCKET_ANIMATION.length) next_frame = 0
    } else {
      deleteSelf()
      tracer.removeTraces(trace)
    }
  }
}*/
