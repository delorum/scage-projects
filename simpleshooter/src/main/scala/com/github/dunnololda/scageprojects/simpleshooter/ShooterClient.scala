package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, ServerDisconnected, ServerConnected, NewServerMessage, NetClient}
import collection.mutable
import collection.mutable.ArrayBuffer

case class ServerData(you:Client, others:List[Client], your_bullets:List[Vec], other_bullets:List[Vec])

object ShooterClient extends ScageScreenApp("Simple Shooter", 640, 480) {
  private val client = NetClient("localhost", 10000)

  private val moves = mutable.HashMap[String, Boolean]("up" -> false, "left" -> false, "down" -> false, "right" -> false)
  private val shoots = ArrayBuffer[Vec]()

  key(KEY_W, onKeyDown = moves("up")    = true, onKeyUp = moves("up")    = false)
  key(KEY_A, onKeyDown = moves("left")  = true, onKeyUp = moves("left")  = false)
  key(KEY_S, onKeyDown = moves("down")  = true, onKeyUp = moves("down")  = false)
  key(KEY_D, onKeyDown = moves("right") = true, onKeyUp = moves("right") = false)

  leftMouse(100, onBtnDown = m => shoots += m)

  def vec(message:NetState):Vec = {
    Vec(message.value[Float]("x").get, message.value[Float]("y").get)
  }

  // send data
  action(100) {
    if(moves.exists(_._2) || shoots.nonEmpty) {
      val builder = ArrayBuffer[(String, Any)]()
      builder ++= moves.filter(_._2)
      if(shoots.nonEmpty) {
        builder += ("shoots" -> shoots.map(v => NetState("x" -> v.x, "y" -> v.y)).toList)
      }
      val state = NetState(builder:_*)
      client.send(state)
      shoots.clear()
    }
  }

  private val positions = mutable.ArrayBuffer[ServerData]()
  private def optRemoveHeadPosition:Option[ServerData] = {
    if(positions.length > 1) Some(positions.remove(0))
    else if (positions.nonEmpty) Some(positions.head)
    else None
  }

  def client(message:NetState):Client = {
    Client(id = message.value[Long]("id").get,
           coord = vec(message),
           health = message.value[Int]("hp").get
    )
  }

  private def serverData(message:NetState):ServerData = {
    val you = client(message.value[NetState]("you").get)
    val others = message.value[List[NetState]]("others").getOrElse(Nil).map(m => client(m))
    val your_bullets = message.value[List[NetState]]("your_bullets").getOrElse(Nil).map(m => vec(m))
    val other_bullets = message.value[List[NetState]]("other_bullets").getOrElse(Nil).map(m => vec(m))
    ServerData(you, others, your_bullets, other_bullets)
  }

  private def wall(message:NetState):Wall = {
    Wall(Vec(message.value[Float]("fromx").get, message.value[Float]("fromy").get), Vec(message.value[Float]("tox").get, message.value[Float]("toy").get))
  }

  private def serverWalls(message:NetState):List[Wall] = {
    message.value[List[NetState]]("walls").get.map(x => wall(x))
  }

  private var is_connected = false

  // receive data
  action(10) {
    client.newEvent {
      case NewServerMessage(message) =>
        println(message.toJsonString)
        if(message.contains("walls")) {
          walls.clear()
          walls ++= serverWalls(message)
        } else {
          val sd = serverData(message)
          //println(sd)
          positions += sd
        }
      case ServerConnected => is_connected = true
      case ServerDisconnected => is_connected = false
    }
  }

  // update state
  /*action(10) {

  }*/

  private val walls = ArrayBuffer[Wall]()

  render {
    if(!is_connected) {
      print("Connecting to Server...", windowCenter, DARK_GRAY, align = "center")
    } else {
      optRemoveHeadPosition match {
        case Some(ServerData(you, others, your_bullets, other_bullets)) =>
          drawCircle(you.coord, 10, RED)
          others.foreach(c => drawCircle(c.coord, 10, WHITE))
          your_bullets.foreach(b => {
            drawRectCentered(b, 3, 3, RED)
          })
          other_bullets.foreach(b => {
            drawRectCentered(b, 3, 3, WHITE)
          })
          print(you.health, 20, 20, WHITE)
        case None =>
      }
      walls.foreach(wall => {
        drawLine(wall.from, wall.to, WHITE)
      })
    }
  }
}
