package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.simplenet.{State => NetState, _}
import collection.mutable
import collection.mutable.ArrayBuffer
import scala.Some

case class ServerData(you:Client, others:List[Client], your_bullets:List[Vec], other_bullets:List[Vec])

object ShooterClient extends ScageScreenApp(s"Simple Shooter v$appVersion", map_width, map_height) {
  private val client = UdpNetClient(address = "localhost", port = 10000, ping_timeout= 500, check_timeout = 1000)

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

  private val states = mutable.ArrayBuffer[ServerData]()
  private def optRemoveHeadState:Option[ServerData] = {
    if(states.length > 1) Some(states.remove(0))
    else if (states.nonEmpty) Some(states.head)
    else None
  }

  def client(message:NetState):Client = {
    Client(id = message.value[Long]("id").get,
           coord = vec(message),
           health = message.value[Int]("hp").get,
           wins = message.value[Int]("w").get,
           deaths = message.value[Int]("d").get,
           visible = message.value[Boolean]("v").get
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
      case NewUdpServerMessage(message) =>
        //println(message.toJsonString)
        if(message.contains("walls")) {
          walls.clear()
          walls ++= serverWalls(message)
        } else {
          val sd = serverData(message)
          //println(sd)
          states += sd
        }
      case UdpServerConnected => is_connected = true
      case UdpServerDisconnected => is_connected = false
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
      optRemoveHeadState match {
        case Some(ServerData(you, others, your_bullets, other_bullets)) =>
          drawCircle(you.coord, 10, RED)
          others.filter(_.visible).zipWithIndex.foreach(c => {
            drawCircle(c._1.coord, 10, WHITE)
            print(c._2+1, c._1.coord, WHITE, align = "center")
          })
          your_bullets.foreach(b => {
            drawRectCentered(b, bullet_size, bullet_size, RED)
          })
          other_bullets.foreach(b => {
            drawRectCentered(b, bullet_size, bullet_size, WHITE)
          })

          walls.filter(w => isWallVisible(w, you.coord, walls.filterNot(ow => ow == w))).foreach(wall => {
            drawLine(wall.from, wall.to, WHITE)
          })

          val other_stats = others.zipWithIndex.map(c => s"${c._2+1}:(${c._1.wins}, ${c._1.deaths})").mkString(" ")
          print(s"[ryou:(${you.wins}, ${you.deaths})] $other_stats", 20, windowHeight-20, WHITE)
          print(s"hp: ${you.health}", 20, 20, RED)
        case None =>
      }
    }
  }
}
