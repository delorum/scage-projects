package net.scage.tutorial.netflight

import net.scage.ScageApp
import net.scage.ScageLib._
import net.scage.support.net.NetServer
import net.scage.support.{State, Vec}
import net.scage.support.tracer3.{Trace, CoordTracer}
import collection.mutable.{HashMap, ArrayBuffer}

object NetFlight extends ScageApp("Net Flight") {
  val tracer = CoordTracer(solid_edges = false)
  private val planes = HashMap[Int, Plane]()

  NetServer.startServer(
    onClientAccepted = {
      client => planes += (client.id -> new Plane(client.id))
    },
    onClientDataReceived = {
      (client, data) =>

    },
    onClientDisconnected = {
      client =>
        planes.getOrElse(client.id, new Plane(0)).stop()
        planes -= client.id
    }
  )
}

trait FlyingObject {
  protected var speed = 5.0f
  protected var rotation = 0.0f

  def step = Vec(-0.4f * speed * math.sin(math.toRadians(rotation)).toFloat,
    0.4f * speed * math.cos(math.toRadians(rotation)).toFloat)
}

import NetFlight._

class Plane(val client_id: Int) extends FlyingObject {
  protected var _health = 100

  def health = _health

  protected var plane_side = 1

  private val trace = tracer.addTrace(windowCenter, new Trace {
    def state = State("type" -> "plane", "health" -> _health)

    def changeState(changer: Trace, s: State) {
      s.neededKeys {
        case ("damage", damage: Int) => _health -= damage
      }
    }
  })

  private val action_id = action {
    if (_health <= 0) {
      _health = 100
      tracer.updateLocation(trace, tracer.randomCoord())
      rotation = (math.random * 20 - 10).toFloat
    }

    tracer.updateLocation(trace, trace.location + step)
    if (speed > 5) speed -= 0.1f
  }

  def stop() {
    delActions(action_id)
    tracer.removeTraces(trace)
  }
}
