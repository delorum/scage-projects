package net.scage.tutorial.netflight

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.net.NetClient
import net.scage.support.{State, Vec}
import collection.mutable.{ArrayBuffer, HashMap}

object NetFlightClient extends ScageScreenApp(unit_name = "Net Flight Client") {
  val ENEMY_PLANE  = image("plane.png", 60, 60, 0, 0, 122, 121)
  val OUR_PLANE    = image("plane2.png", 60, 60, 0, 0, 122, 121)
  val ROCKET_ANIMATION = animation("rocket_animation.png", 10, 29, 14, 44, 3)
  val EXPLOSION_ANIMATION = animation("explosion_animation.png", 36, 35, 72, 69, 3)
  val LAND         = image("land.png", 800, 600, 0, 0, 800, 600)
  val send_timeout = property("netflight.send_timeout", 50)

  private val planes = HashMap[Int, ClientPlane]()
  
  val rockets = HashMap[Int, ClientRocket]()
  
  private var our_plane_id = -1
  private var our_plane:Option[ClientPlane] = None

  NetClient.startClient(
    onServerDataReceived = {
      data => data.neededKeys {
        case ("planes", planes_data:List[State]) =>
          planes_data.foreach(plane_data => plane_data match {
            case State(("coord", coord:Vec),
                       ("health", health:Float),
                       ("id", id:Float),
                       ("rotation", rotation:Float),
                       ("speed", speed:Float)) =>
              planes.get(id.toInt) match {
                case Some(plane) => plane.update(coord, rotation, speed, health.toInt)
                case None =>
                  val new_plane = if(id.toInt == our_plane_id) {
                    val plane = new ClientPlane(id.toInt, coord, rotation, speed, health.toInt, OUR_PLANE)
                    our_plane = Some(plane)
                    plane
                  } else new ClientPlane(id.toInt, coord, rotation, speed, health.toInt)
                  planes += (id.toInt -> new_plane)
              }
            //case _ =>
        })
        case ("rockets", rockets_data:List[State]) =>
          rockets_data.foreach(rocket_data => rocket_data match {
            case State(("coord", coord:Vec),
                       ("fuel", fuel:Float),
                       ("id", id:Float),
                       ("rotation", rotation:Float),
                       ("speed", speed:Float)) =>
              rockets.get(id.toInt) match {
                case Some(rocket) => rocket.update(coord, rotation, speed, fuel.toInt)
                case None =>
                  val new_rocket = new ClientRocket(id.toInt, coord, rotation, speed, fuel.toInt)
                  rockets += (id.toInt -> new_rocket)
              }
            //case _ =>
          })
        case ("your_plane_id", id:Float) =>
          our_plane_id = id.toInt
        case ("leaver", id:Float) =>
          planes.get(id.toInt).map(_.stop())
          planes -= id.toInt
      }        
    }
  )

  val LEFT = 0
  val RIGHT = 1
  val UP = 2
  val LCTRL = 3
  private val keyboard_buffer = ArrayBuffer[Int]()
  key(KEY_LEFT,  10, onKeyDown = {
    keyboard_buffer += LEFT
    our_plane.map(_.rotateLeft())
  })
  key(KEY_RIGHT, 10, onKeyDown = {
    keyboard_buffer += RIGHT
    our_plane.map(_.rotateRight())
  })
  key(KEY_UP, 10, onKeyDown = {
    keyboard_buffer += UP
    our_plane.map(_.reheat())
  })
  
  key(KEY_LCONTROL, onKeyDown = {
    keyboard_buffer += LCTRL
    //our_plane.map(_.fire())
  })

  action(send_timeout) {
    NetClient.send(State("keys" -> keyboard_buffer.toList))
    keyboard_buffer.clear()
  }
  
  backgroundColor = WHITE
  render(-10) {
    drawDisplayList(LAND, windowCenter)
  }

  dispose {
    NetClient.stopClient()
  }
}

import NetFlightClient._

class ClientPlane(val client_id: Int, 
                  init_coord:Vec, 
                  init_rotation:Float, 
                  init_speed:Float, 
                  init_health:Int,
                  image_code:Int = ENEMY_PLANE) extends FlyingObject {
  _rotation = init_rotation
  _speed = init_speed
  
  private var _coord = init_coord
  def coord = _coord
  
  protected var _health = init_health
  def health = _health
  
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
  
  private val action_id = action(10) {
    _coord = from + to_minus_from*(System.currentTimeMillis() - from_start_time)/send_timeout
  }
  
  private val render_id = render {
    openglMove(_coord)
    openglLocalTransform {
      openglRotate(_rotation)
      drawDisplayList(image_code, Vec.zero)
    }
    print(_health, Vec.zero, YELLOW)
  }
  
  def stop() {
    delOperations(action_id, render_id)
  }
}

class ClientRocket(val rocket_id: Int, 
                   init_coord:Vec, 
                   init_rotation:Float, 
                   init_speed:Float, 
                   init_fuel:Int) extends FlyingObject {
  _rotation = init_rotation
  _speed = init_speed
  
  private var _coord = init_coord
  def coord = _coord
  
  protected var _fuel = init_fuel
  def fuel = _fuel
  
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
  def update(new_coord:Vec, new_rotation:Float, new_speed:Float, new_fuel:Int) {
    _rotation = new_rotation
    _speed = new_speed
    _fuel = new_fuel
    coords += new_coord
    if(coords.length >= 3) {
      val new_from = coords(coords.length - 3)
      val new_to = coords(coords.length - 2)
      updateFromTo(new_from, new_to)
      coords -= coords(0)
    }
  }
  
  action(10) {
    if(_fuel > 0) {
      _coord = from + to_minus_from*(System.currentTimeMillis() - from_start_time)/send_timeout
      _fuel -= 1
    } else {
      deleteSelf()
      rockets -= rocket_id
    }
  }
  
  render {
    if(fuel > 0) {
      openglMove(_coord)
      openglRotate(rotation)
      drawDisplayList(ROCKET_ANIMATION(0), Vec.zero)
    } else {
      openglMove(_coord)
      drawDisplayList(EXPLOSION_ANIMATION(0), Vec.zero)
      deleteSelf()
    }
  }
}