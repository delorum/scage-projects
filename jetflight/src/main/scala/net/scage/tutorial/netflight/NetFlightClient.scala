package net.scage.tutorial.netflight

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.tracer3.{Trace, CoordTracer}
import net.scage.support.net.NetClient
import net.scage.support.{State, Vec}
import collection.mutable.{ArrayBuffer, HashMap}

object NetFlightClient extends ScageScreenApp(unit_name = "Net Flight Client") {
  val PLAYER_PLANE = image("plane.png", 60, 60, 0, 0, 122, 121)
  val LAND         = image("land.png", 800, 600, 0, 0, 800, 600)
  val send_timeout = property("netflight.send_timeout", 50)

  private val planes = HashMap[Int, ClientPlane]()
  private var our_plane_id = -1
  private var our_plane:Option[ClientPlane] = None

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
        case ("your_plane_id", id:Float) =>
          our_plane_id = id.toInt
        case ("plane", State(("coord", coord:Vec),
                             ("health", health:Float), 
                             ("id", id:Float), 
                             ("rotation", rotation:Float), 
                             ("speed", speed:Float))) =>
          planes.get(id.toInt) match {
            case Some(plane) => plane.update(coord, rotation, speed, health.toInt)
            case None =>
              val new_plane = new ClientPlane(id.toInt, coord, rotation, speed, health.toInt)
              if(id.toInt == our_plane_id) our_plane = Some(new_plane)
              planes += (id.toInt -> new_plane)
          }
      }        
    }
  )

  val LEFT = 0
  val RIGHT = 1
  val UP = 2
  private val keyboard_buffer = ArrayBuffer[Int]()
  key(KEY_LEFT,  10, onKeyDown = {
    keyboard_buffer += LEFT
    our_plane.map(_.rotateLeft())
  })
  key(KEY_RIGHT, 10, onKeyDown = {
    keyboard_buffer += RIGHT
    our_plane.map(_.rotateRight())
  })
  key(KEY_UP,    10, onKeyDown = {
    keyboard_buffer += UP
    our_plane.map(_.reheat())
  })

  action(send_timeout) {
    NetClient.send(State("keys" -> keyboard_buffer.toList))
    keyboard_buffer.clear()
  }
  
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

  def rotateLeft() {
    _rotation += 0.2f*_speed
  }

  def rotateRight() {
    _rotation -= 0.2f*_speed
  }

  def reheat() {
    if(_speed < 15) _speed += 0.5f
  }
  
  private var from_start_time = System.currentTimeMillis()
  private var from = _coord
  private var to_minus_from = Vec.zero
  private def updateFromTo(new_from:Vec, new_to:Vec) {
    from = new_from
    if((new_to - new_from)*step > 0) {
      to_minus_from = new_to - new_from
      _coord = from
      from_start_time = System.currentTimeMillis()
    }
  }
  
  private val coords = ArrayBuffer[Vec]()
  def update(new_coord:Vec, new_rotation:Float, new_speed:Float, new_health:Int) {
    _rotation = new_rotation
    _speed = new_speed
    _health = new_health
    coords += new_coord
    if(coords.length >= 3) {
      val new_from = coords(coords.length - 3)
      val new_to = coords(coords.length - 2)
      updateFromTo(new_from, new_to)
      coords -= coords(0)
    }
  }
  
  action {
    _coord = from + to_minus_from*(System.currentTimeMillis() - from_start_time)/send_timeout
  }
  
  render {
    openglMove(_coord)
    openglRotate(_rotation)
    drawDisplayList(PLAYER_PLANE, Vec.zero)
  }
}