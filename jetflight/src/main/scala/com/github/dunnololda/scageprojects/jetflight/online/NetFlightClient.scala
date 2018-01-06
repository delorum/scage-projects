package com.github.dunnololda.scageprojects.jetflight.online

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet._
import play.api.libs.json._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class NetFlightClientData(keys:Option[List[Int]], goodbye:Option[Boolean])

object NetFlightClient extends ScageScreenApp("Net Flight Client") {
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

  //center = our_plane.map(_.coord).getOrElse(windowCenter)
  
  private val count = mutable.HashMap[Int, (Int,  Int)]() // client -> (wins, deaths)
  interface {
    for {
      ((client_id, (wins, loses)), index) <- count.zipWithIndex
      is_our_count = client_id == our_plane_id
    } print(client_id+": "+wins+" / "+loses, 20, windowHeight-20-20*index, if(is_our_count) RED else GREEN)

    print("Health: "+(if(our_plane.isDefined) our_plane.get.health else 0), 20, 20, RED)
  }

  val client = UdpNetClient(address = "localhost", port = 15000)

  implicit val VecJson_reader = {
    case class VecJson(x:Float, y:Float)
    import play.api.libs.functional.syntax._
    (
      (__ \ "x").read[Float] and
      (__ \ "y").read[Float]
    )(VecJson.apply _).map(z => Vec(z.x, z.y))
  }
  implicit val ServerDataCountUpdate_reader = Json.reads[ServerDataCountUpdate]
  implicit val ServerObjectType_reader = EnumUtils.enumReads(ServerObjectType)
  implicit val ServerDataPlane_reader = Json.reads[ServerDataPlane]
  implicit val ServerDataRocket_reader = Json.reads[ServerDataRocket]
  implicit val ServerDataWord_reader = Json.reads[ServerDataWord]
  implicit val ServerDataHealthBar_reader = Json.reads[ServerDataHealthBar]
  implicit val ServerData_reader = Json.reads[ServerData]

  val LEFT = 0
  val RIGHT = 1
  val UP = 2
  val LCTRL = 3
  private val keyboard_buffer = ArrayBuffer[Int]()
  key(KEY_LEFT,  10, onKeyDown = {
    keyboard_buffer += LEFT
    our_plane.foreach(_.rotateLeft())
  })
  key(KEY_RIGHT, 10, onKeyDown = {
    keyboard_buffer += RIGHT
    our_plane.foreach(_.rotateRight())
  })
  key(KEY_UP, 10, onKeyDown = {
    keyboard_buffer += UP
    our_plane.foreach(_.reheat())
  })
  
  key(KEY_LCONTROL, 200, onKeyDown = {
    keyboard_buffer += LCTRL
    //our_plane.map(_.fire())
  })

  actionStaticPeriod(send_timeout) {
    client.send(Json.obj("keys" -> keyboard_buffer.toList))
    keyboard_buffer.clear()
  }

  backgroundColor = BLACK
  /*render(-10) {
    drawDisplayList(LAND, windowCenter)
  }*/

  actionStaticPeriod(10) {
    client.newEvent {
      case NewUdpServerData(data) =>
        data.validate[ServerData] match {
          case JsSuccess(server_data, _) =>
            server_data.planes_rockets.foreach(pr => pr.foreach {
              case server_object =>
                (server_object \ "object_type").validate[ServerObjectType.Value] match {
                  case JsSuccess(object_type, _) =>
                    object_type match {
                      case  ServerObjectType.Plane =>
                        server_object.validate[ServerDataPlane] match {
                          case JsSuccess(server_plane, _) =>
                            planes.get(server_plane.client_id.toInt) match {
                              case Some(plane) => plane.update(server_plane.coord, server_plane.rotation, server_plane.speed, server_plane.health)
                              case None =>
                                val new_plane = if(server_plane.client_id.toInt == our_plane_id) {
                                  val plane = new ClientPlane(server_plane.client_id.toInt, server_plane.coord, server_plane.rotation, server_plane.speed, server_plane.health, OUR_PLANE, RED)
                                  our_plane = Some(plane)
                                  plane
                                } else new ClientPlane(server_plane.client_id.toInt, server_plane.coord, server_plane.rotation, server_plane.speed, server_plane.health)
                                planes += (server_plane.client_id.toInt -> new_plane)
                            }
                          case JsError(error) =>
                            println(s"[client] failed to parse server plane: $data, error: $error")
                        }
                      case  ServerObjectType.Rocket =>
                        server_object.validate[ServerDataRocket] match {
                          case JsSuccess(server_rocket, _) =>
                            rockets.get(server_rocket.rocket_id) match {
                              case Some(rocket) => rocket.update(server_rocket.coord, server_rocket.rotation, server_rocket.speed, server_rocket.fuel)
                              case None =>
                                val new_rocket = new ClientRocket(server_rocket.rocket_id, server_rocket.coord, server_rocket.rotation, server_rocket.speed, server_rocket.fuel, if(server_rocket.client_id.toInt == our_plane_id) OUR_ROCKET else ENEMY_ROCKET)
                                rockets += (server_rocket.rocket_id -> new_rocket)
                            }
                          case JsError(error) =>
                            println(s"[client] failed to parse server rocket: $data, error: $error")
                        }
                      case  ServerObjectType.Word =>
                        server_object.validate[ServerDataWord] match {
                          case JsSuccess(server_word, _) =>
                            if(!words.contains(server_word.word_id)) {
                              val new_word = new ClientFlyingWord(server_word.word_id, server_word.message, server_word.coord, server_word.dir, if(server_word.client_id.toInt == our_plane_id) RED else GREEN)
                              words += (server_word.word_id -> new_word)
                            }
                          case JsError(error) =>
                            println(s"[client] failed to parse server word: $data, error: $error")
                        }
                      case  ServerObjectType.Health =>
                        server_object.validate[ServerDataHealthBar] match {
                          case JsSuccess(server_health_bar, _) =>
                            health_bars.get(server_health_bar.health_id) match {
                              case Some(health_bar) => health_bar.addLifeTime()
                              case None =>
                                val new_health_bar = new ClientHealthBar(server_health_bar.health_id, server_health_bar.coord)
                                health_bars += (server_health_bar.health_id -> new_health_bar)
                            }
                          case JsError(error) =>
                            println(s"[client] failed to parse server health bar: $data, error: $error")
                        }
                    }
                  case JsError(error) =>
                    println(s"[client] failed to parse server object: $server_object, error: $error")
                }
            })
            server_data.count.foreach {
              case count_updates =>
                count_updates.foreach {
                  case ServerDataCountUpdate(client_id, wins, loses) =>
                    count += (client_id.toInt ->(wins.toInt, loses.toInt))
                }
            }
            server_data.your_plane_id.foreach {
              case x =>
                our_plane_id = x.toInt
            }
            server_data.leaver.foreach {
              case x =>
                planes.get(x.toInt).foreach(_.stop())
                planes -= x.toInt
                count -= x.toInt
            }
          case JsError(error) =>
            println(s"[client] failed to parse server data: $data, error: $error")
        }
    }
  }

  dispose {
    client.send(Json.obj("goodbye" -> true))
    client.stop()

    /*Thread.sleep(5000)*/
  }
}

import com.github.dunnololda.scageprojects.jetflight.online.NetFlightClient._

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
      coords -= coords.head
    }
  }
  
  private val action_id = actionStaticPeriod(10) {
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
      coords -= coords.head
    }
  }
  
  actionStaticPeriod(10) {
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

  actionStaticPeriod(10) {
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

  actionStaticPeriod(10) {
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