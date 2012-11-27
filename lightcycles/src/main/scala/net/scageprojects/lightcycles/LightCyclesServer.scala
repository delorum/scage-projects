package net.scageprojects.lightcycles

import net.scage.ScageApp
import net.scage.support.net.NetServer
import net.scage.ScageLib._
import actors.Actor._
import collection.mutable
import net.scage.support.{ScageColor, Vec, State}
import net.scage.support.tracer3.{DefaultTrace, CoordTracer}
import scala.actors.TIMEOUT

object North extends Vec( 0, 1)
object South extends Vec( 0,-1)
object East  extends Vec( 1, 0)
object West  extends Vec(-1, 0)

class LightCycle(val client_id:Int, val color:ScageColor, var dir:Vec) extends DefaultTrace {
  val prev_coords = mutable.ArrayBuffer[Vec]()
  override def state = State("client_id" -> client_id, "color" -> color, "location" -> location)
}

object LightCyclesServer extends ScageApp("Light Cycles Server") {
  val tracer = CoordTracer.create[LightCycle](
    field_from_x = 10,
    field_to_x   = 640-10,
    field_from_y = 10,
    field_to_y   = 480-10,
    solid_edges  = true
  )
  val speed = 1
  val prev_coords_length = 200

  action(10) {
    val start = msecs
    def _receive() {
      self.receiveWithin(0) {
        case ("new client", client_id:Int) =>
          val pos = tracer.randomCoord()
          val dir = (math.random*4).toInt match {
            case 0 => North
            case 1 => South
            case 2 => East
            case 3 => West
            case _ => North
          }
          tracer.addTrace(pos, new LightCycle(client_id, randomColor, dir))
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
        case ("leaver", client_id:Int) =>
          val cycle = tracer.tracesList.find(cycle => cycle.client_id == client_id).get
          tracer.removeTracesById(cycle.id)
          NetServer.sendToAll(State("leaver" -> client_id))
        case TIMEOUT =>
      }
      if(msecsFrom(start) < 10) _receive()
    }
    _receive()
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
  }

  action(50) {
    val cycles_state = tracer.tracesList.map(_.state)
    NetServer.sendToAll(State("cycles" -> cycles_state))
  }

  /*render {
    tracer.tracesList.foreach(cycle => {
      drawSlidingLines(cycle.prev_coords, cycle.color)
    })
  }*/

  val scage_actor = self
  NetServer.startServer(
    offline_check_timeout = 10,
    onClientAccepted = (client) => {
      scage_actor ! (("new client", client.id))
    },
    onClientDataReceived = (client, data) => {
      scage_actor ! (("new client data", client.id, data))
    },
    onClientDisconnected = (client) => {
        scage_actor ! (("leaver", client.id))
    }
  )
}
