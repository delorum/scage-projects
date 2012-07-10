package net.scage.tutorial.netflight

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.net.NetClient
import collection.mutable.{ArrayBuffer, HashMap}
import net.scage.support.{ScageColor, State, Vec}
import collection.mutable

object NetFlightClient extends ScageScreenApp(unit_name = "Net Flight Client") {
  val OUR_PLANE    = displayList {drawPolygon(Array(Vec(-15,-28), Vec(0,25), Vec(15,-28), Vec(0,-15)), RED)}
  val ENEMY_PLANE  = displayList {drawPolygon(Array(Vec(-15,-28), Vec(0,25), Vec(15,-28), Vec(0,-15)), GREEN)}
  val OUR_ROCKET   = displayList {drawRect(Vec(-2,4), 4, 8, RED)}
  val ENEMY_ROCKET = displayList {drawRect(Vec(-2,4), 4, 8, GREEN)}

  val send_timeout = property("netflight.send_timeout", 50)

  private val planes = mutable.HashMap[Int, ClientPlane]()
  val rockets        = mutable.HashMap[Int, ClientRocket]()
  val words          = mutable.HashMap[Int, ClientFlyingWord]()
  val health_bars    = mutable.HashMap[Int, ClientHealthBar]()

  private var our_plane_id = -1
  private var our_plane:Option[ClientPlane] = None
  
  private val count = mutable.HashMap[Int, (Int,  Int)]() // client -> (wins, deaths)
  interface {
    for {
      ((client_id, (wins, loses)), index) <- count.zipWithIndex
      is_our_count = client_id == our_plane_id
    } print(client_id+": "+wins+" / "+loses, 20, windowHeight-20-20*index, if(is_our_count) RED else GREEN)

    print("Health: "+(if(our_plane.isDefined) our_plane.get.health else 0), 20, 20, RED)
  }

  NetClient.startClient(
    onServerDataReceived = {
      data => synchronized {data.neededKeys {
        case ("planes_rockets", planes_data:List[State]) =>
          planes_data.foreach(plane_data => plane_data match {
            case State(("client_id", client_id:Float),
                       ("coord", coord:Vec),
                       ("health", health:Float),
                       ("rotation", rotation:Float),
                       ("speed", speed:Float),
                       ("type", "plane")) =>
              planes.get(client_id.toInt) match {
                case Some(plane) => plane.update(coord, rotation, speed, health.toInt)
                case None =>
                  val new_plane = if(client_id.toInt == our_plane_id) {
                    val plane = new ClientPlane(client_id.toInt, coord, rotation, speed, health.toInt, OUR_PLANE, RED)
                    our_plane = Some(plane)
                    plane
                  } else new ClientPlane(client_id.toInt, coord, rotation, speed, health.toInt)
                  planes += (client_id.toInt -> new_plane)
              }
            case State(("client_id", client_id:Float),
                       ("coord", coord:Vec),
                       ("fuel", fuel:Float),
                       ("rocket_id", rocket_id:Float),
                       ("rotation", rotation:Float),
                       ("speed", speed:Float),
                       ("type", "rocket")) =>
              rockets.get(rocket_id.toInt) match {
                case Some(rocket) => rocket.update(coord, rotation, speed, fuel.toInt)
                case None =>
                  val new_rocket = new ClientRocket(rocket_id.toInt, coord, rotation, speed, fuel.toInt, if(client_id.toInt == our_plane_id) OUR_ROCKET else ENEMY_ROCKET)
                  rockets += (rocket_id.toInt -> new_rocket)
              }
            case State(("client_id", client_id:Float),
                       ("coord", coord:Vec),
                       ("dir", dir:Vec),
                       ("message", message:String),
                       ("type", "word"),
                       ("word_id", word_id:Float)) =>
              if(!words.contains(word_id.toInt)) {
                val new_word = new ClientFlyingWord(word_id.toInt, message, coord, dir, if(client_id.toInt == our_plane_id) RED else GREEN)
                words += (word_id.toInt -> new_word)
              }
            case State(("coord", coord:Vec),
                       ("health_id", health_id:Float),
                       ("type", "health")) =>
              health_bars.get(health_id.toInt) match {
                case Some(health_bar) => health_bar.addLifeTime()
                case None =>
                  val new_health_bar = new ClientHealthBar(health_id.toInt, coord)
                  health_bars += (health_id.toInt -> new_health_bar)
              }
            //case _ =>
        })
        case ("count", count_data:List[State]) =>
          count_data.foreach(count_for_client => count_for_client match {
            case State(("client_id", client_id:Float), 
                       ("loses", loses:Float), 
                       ("wins", wins:Float)) =>
              count += (client_id.toInt -> (wins.toInt, loses.toInt))
          })          
        case ("your_plane_id", id:Float) =>
          our_plane_id = id.toInt
        case ("leaver", id:Float) =>
          planes.get(id.toInt).map(_.stop())
          planes -= id.toInt
          count -= id.toInt
      }        
    }}
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
  
  key(KEY_LCONTROL, 200, onKeyDown = {
    keyboard_buffer += LCTRL
    //our_plane.map(_.fire())
  })

  action(send_timeout) {
    NetClient.send(State("keys" -> keyboard_buffer.toList))
    keyboard_buffer.clear()
  }

  backgroundColor = BLACK
  /*render(-10) {
    drawDisplayList(LAND, windowCenter)
  }*/

  dispose {
    NetClient.sendSync(State("goodbye"))
    NetClient.stopClient()

    /*Thread.sleep(5000)*/
  }
}

import NetFlightClient._

class ClientPlane(val client_id: Int, 
                  init_coord:Vec, 
                  init_rotation:Float, 
                  init_speed:Float, 
                  init_health:Int,
                  image_code:Int = ENEMY_PLANE, health_color:ScageColor = GREEN) extends FlyingObject {
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
    //drawFilledRect(Vec(-30,-30), 60f*(_health/100), 5, health_color)
  }
  
  def stop() {
    delOperations(action_id, render_id)
  }
}

class ClientRocket(val rocket_id: Int,
                   init_coord:Vec, 
                   init_rotation:Float, 
                   init_speed:Float, 
                   init_fuel:Int,
                   rocket_image_code:Int) extends FlyingObject {
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
      drawDisplayList(rocket_image_code, Vec.zero)
    } else {
      /*openglMove(_coord)
      drawDisplayList(EXPLOSION_ANIMATION(0), Vec.zero)*/
      deleteSelf()
    }
  }
}

class ClientFlyingWord(rocket_id:Int, message:Any, init_coord:Vec, direction:Vec, color:ScageColor) extends FlyingObject {
  private var _lifetime = 60
  private var _coord = init_coord
  private val _dir = direction.n

  action(10) {
    if(_lifetime > 0) {
      _coord += _dir
      _lifetime -= 1
    } else {
      deleteSelf()
      words -= rocket_id
    }
  }

  render {
    if(_lifetime > 0) {
      print(message, _coord, color)
    } else deleteSelf()
  }
}

class ClientHealthBar(health_id:Int, coord:Vec) {
  private var _lifetime = 30

  def addLifeTime() {
    _lifetime = 30
  }

  action(10) {
    _lifetime -= 1
    if(_lifetime <= 0) {
      deleteSelf()
      health_bars -= health_id
    }
  }

  render {
    if(_lifetime > 0) {
      openglMove(coord)
      drawRectCentered(Vec.zero, 30, 30, RED)
      drawLines(Vec(0, 13), Vec(0, -13),
                Vec(-13, 0), Vec(13, 0))
    } else deleteSelf()
  }
}