package net.scage.tutorial.netflight

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import collection.mutable.HashMap
import net.scage.support.tracer3.{Trace, CoordTracer}
import net.scage.support.net.NetClient
import net.scage.support.{State, Vec}

object NetFlightClient extends ScageScreenApp(unit_name = "Net Flight Client") {
  val PLAYER_PLANE = image("plane.png", 60, 60, 0, 0, 122, 121)
  val LAND         = image("land.png", 800, 600, 0, 0, 800, 600)

  private val planes = HashMap[Int, ClientPlane]()
  
  NetClient.startClient(
    port = 9800,
    onServerDataReceived = {
      data => data.neededKeys {
        case ("planes", planes_data:List[State]) =>
          planes_data.foreach(plane_data => {
            plane_data match {
              case State(("coord", coord:Vec),
                         ("health", health:Float),
                         ("id", id:Float),
                         ("rotation", rotation:Float),
                         ("speed", speed:Float)) =>
                planes.get(id.toInt) match {
                  case Some(plane) => plane.update(coord, rotation, speed, health.toInt)
                  case None => planes += (id.toInt -> new ClientPlane(id.toInt, coord, rotation, speed, health.toInt))
                }
              case _ =>
            }
          })
        case ("plane", State(("coord", coord:Vec), 
                             ("health", health:Float), 
                             ("id", id:Float), 
                             ("rotation", rotation:Float), 
                             ("speed", speed:Float))) =>
          planes.get(id.toInt) match {
            case Some(plane) => plane.update(coord, rotation, speed, health.toInt)
            case None => planes += (id.toInt -> new ClientPlane(id.toInt, coord, rotation, speed, health.toInt))
          }
      }        
    }
  )
  
  key(KEY_LEFT,  10, onKeyDown = NetClient.send(State("left")))
  key(KEY_RIGHT, 10, onKeyDown = NetClient.send(State("right")))
  key(KEY_UP,    10, onKeyDown = NetClient.send(State("up")))
  
  backgroundColor = WHITE
  render(-10) {
    drawDisplayList(LAND, windowCenter)
  }
}

import NetFlightClient._

class ClientPlane(val client_id: Int, 
                  init_coord:Vec, 
                  init_rotation:Float, 
                  init_speed:Float, 
                  init_health:Int) extends FlyingObject {
  _rotation = init_rotation
  _speed = init_speed
  
  private var _coord = init_coord
  def coord = _coord
  
  protected var _health = init_health
  def health = _health
  
  def update(new_coord:Vec, new_rotation:Float, new_speed:Float, new_health:Int) {
    _coord = new_coord
    _rotation = new_rotation
    _speed = new_speed
    _health = new_health
  }
  
  action(40) {
    _coord += step
    if (_speed > 5) _speed -= 0.1f
  }
  
  render {
    openglMove(_coord)
    openglRotate(_rotation)
    drawDisplayList(PLAYER_PLANE, Vec.zero)
  }
}