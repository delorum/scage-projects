package net.scageprojects.lightcycles

import com.github.dunnololda.scage.ScageScreenApp
import com.github.dunnololda.scage.ScageLib._
import actors.Actor._
import com.github.dunnololda.scage.support.net.NetClient
import collection.mutable
import com.github.dunnololda.scage.support.{ScageColor, Vec}
import actors.TIMEOUT

object LightCyclesClient extends ScageScreenApp("Light Cycles Client", 640, 480) {
  val cycles = mutable.HashMap[Int, LightCycleClient]()
  val prev_coords_length = 200

  action(10) {
    self.receiveWithin(0) {
      case ("new server data", data:State) =>
        data.neededKeys {
          case ("cycles", cycles_data:List[State]) =>
            cycles_data.foreach {
              case State(("client_id", client_id: Float),
              ("color", color: ScageColor),
              ("location", location: Vec)) =>
                cycles.get(client_id.toInt) match {
                  case Some(cycle) =>
                    cycle.update(location)
                  case None => cycles += (client_id.toInt -> new LightCycleClient(client_id.toInt, location, color))
                }
            }
          case ("leaver", client_id:Float) =>
            cycles(client_id.toInt).stop()
            cycles -= client_id.toInt
        }
      case TIMEOUT =>
    }
  }

  private var dir = -1
  key(KEY_W, onKeyDown = dir = 0)
  key(KEY_A, onKeyDown = dir = 3)
  key(KEY_S, onKeyDown = dir = 1)
  key(KEY_D, onKeyDown = dir = 2)
  action(50) {
    if(dir != -1) {
      NetClient.send(State("dir" -> dir))
      dir = -1
    }
  }

  val scage_actor = self
  NetClient.startClient(
    onServerDataReceived = (data) => {
      scage_actor ! (("new server data", data))
    }
  )
}

import LightCyclesClient._

class LightCycleClient(val client_id:Int, init_coord:Vec, val color:ScageColor) {
  val prev_coords = mutable.ArrayBuffer[Vec]()
  private var _coord = init_coord

  private var from_start_time = System.currentTimeMillis()
  private var from = _coord
  private var to_minus_from = Vec.zero
  private def updateFromTo(new_from:Vec, new_to:Vec) {
    from = new_from
    to_minus_from = new_to - new_from
    _coord = from
    from_start_time = System.currentTimeMillis()
  }

  private val coords = mutable.ArrayBuffer[Vec]()
  def update(new_coord:Vec) {
    coords += new_coord
    if(coords.length >= 3) {
      val new_from = coords(coords.length - 3)
      val new_to = coords(coords.length - 2)
      updateFromTo(new_from, new_to)
      coords -= coords(0)
    }
  }

  private val action_id = action(10) {
    prev_coords += _coord
    if(prev_coords.length >= prev_coords_length) prev_coords.remove(0)
    _coord = from + to_minus_from*(System.currentTimeMillis() - from_start_time)/50
  }

  private val render_id = render {
    drawSlidingLines(prev_coords, color)
  }

  def stop() {
    delOperations(action_id, render_id)
  }
}
