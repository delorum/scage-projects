package net.scageprojects.lightcycles

import com.github.dunnololda.scage.ScageScreenApp
import com.github.dunnololda.scage.support.{State, Vec, ScageColor}
import com.github.dunnololda.scage.support.tracer3.{CoordTracer, DefaultTrace}
import collection.mutable
import actors.Actor._
import com.github.dunnololda.scage.ScageLib._
import actors.TIMEOUT
import com.github.dunnololda.scage.support.net.NetServer

class LightCycleDemo(val client_id:Int, val color:ScageColor, var dir:Vec) extends DefaultTrace {
  val prev_coords = mutable.ArrayBuffer[Vec]()
  override def state = State("client_id" -> client_id, "color" -> color, "location" -> location)
}

object LightCyclesDemo extends ScageScreenApp("Light Cycles Server", 640, 480) {
  val tracer = CoordTracer.create[LightCycleDemo](solid_edges = true)
  val speed = 1
  val prev_coords_length = 200

  action(10) {
    self.receiveWithin(10) {
      case ("new client", client_id:Int) =>
        val pos = tracer.randomCoord()
        val dir = (math.random*4).toInt match {
          case 0 => North
          case 1 => South
          case 2 => East
          case 3 => West
          case _ => North
        }
        tracer.addTrace(pos, new LightCycleDemo(client_id, RED, dir))
      case ("new client data", client_id:Int, data:State) => {
        val cycle = tracer.tracesList.find(cycle => cycle.client_id == client_id).get
        data.neededKeys {
          case ("dir", dir_code:Float) =>
            cycle.dir = dir_code.toInt match {
              case 0 if cycle.dir != South => North
              case 1 if cycle.dir != North => South
              case 2 if cycle.dir != West => East
              case 3 if cycle.dir != East => West
              case _ => cycle.dir
            }
        }
      }
      case TIMEOUT =>
    }
  }

  action(10) {
    tracer.tracesList.foreach(cycle => {
      val new_pos = cycle.location + cycle.dir*speed
      if(tracer.isCoordOnArea(new_pos)) {
        cycle.prev_coords += cycle.location
        if(cycle.prev_coords.length >= prev_coords_length) cycle.prev_coords.remove(0)
        tracer.updateLocation(cycle.id, new_pos)
      }
    })

    /*val cycles_state = tracer.tracesList.map(_.state)
    NetServer.sendToAll(State("cycles" -> cycles_state))*/
  }

  render {
    tracer.tracesList.foreach(cycle => {
      drawSlidingLines(cycle.prev_coords, cycle.color)
    })
  }

  val scage_actor = self
  NetServer.startServer(
    onClientAccepted = (client) => {
      scage_actor ! (("new client", client.id))
    },
    onClientDataReceived = (client, data) => {
      scage_actor ! (("new client data", client.id, data))
    }
  )
}
