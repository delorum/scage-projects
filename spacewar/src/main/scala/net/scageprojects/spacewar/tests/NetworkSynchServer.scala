package net.scageprojects.spacewar.tests

import collection.mutable.ArrayBuffer
import collection.mutable
import net.scage.support.tracer3.TraceTrait
import net.scage.{ScageScreenApp, ScageApp}
import net.scage.support.{Vec, State}
import net.scage.ScageLib._
import net.scage.support.net.{NetClient, NetServer}

trait OutObject {
  def networkId:String
  def objectType:String
  def isChanged:Boolean
  def changesSent()
  def state:State
}

trait OutList[A <: OutObject] extends OutObject {
  this:Seq[A] =>
  def networkId:String
  val objectType = "out_list"

  def state:State = State("network_id" -> networkId, "list" -> zipWithIndex.map {
    case (elem, index) => State("network_id" -> elem.networkId, "position" -> index, "state" -> elem.state)
  }.toList)

  def delta:State = State("network_id" -> networkId, "list" -> zipWithIndex.filter(_._1.isChanged).map {
    case (elem, index) => State("network_id" -> elem.networkId, "position" -> index, "state" -> elem.state)
  }.toList)

  def isChanged = exists(_.isChanged)

  def changesSent() {foreach(_.changesSent())}
}

class MyServerNumber(val networkId:String) extends OutObject {
  val objectType = "my_number"

  private var _number = 0
  def number = _number
  def number_=(new_number:Int) {
    is_changed = new_number != _number
    _number = new_number
  }

  def state = State("number" -> number)

  private var is_changed = false
  def isChanged = is_changed
  def changesSent() {is_changed = false}
}

object NetworkSynchServer extends ScageScreenApp("Network Synch Server", 640, 480) {
  private val out_objects = mutable.HashMap[String, OutObject]()
  private val out_lists = mutable.HashMap[String, OutList[_]]()

  private val my_numbers = new ArrayBuffer[MyServerNumber] with OutList[MyServerNumber] {
    val networkId = "my_numbers"
  }
  my_numbers ++= (0 to 9).map(i => new MyServerNumber(nextId.toString))

  out_lists += (my_numbers.networkId -> my_numbers)

  out_objects += (MyServerCircle.networkId -> MyServerCircle)

  NetServer.startServer(
    onClientAccepted = {client =>
      val delta = out_lists.values.map(_.state).toList ++
                  out_objects.values.map(elem => State("object" -> State("network_id" -> elem.networkId, "object_type" -> elem.objectType, "state" -> elem.state)))
      client.send(State("delta" -> delta))
    }
  )

  action(30) {
    val changed_lists = out_lists.values.filter(_.isChanged)
    val changed_objects = out_objects.values.filter(_.isChanged)
    val delta = (changed_lists.map(_.delta) ++ changed_objects.map(elem =>
      State("object" -> State("network_id" -> elem.networkId, "object_type" -> elem.objectType, "state" -> elem.state)))).toList
    if(delta.nonEmpty) {
      NetServer.sendToAll(State("delta" -> delta))
      changed_lists.foreach(_.changesSent())
      changed_objects.foreach(_.changesSent())
    }
  }

  backgroundColor = BLACK
  render {
    val str = my_numbers.map(_.number).mkString(" ")
    printCentered(str, windowCenter, WHITE)
  }

  key(KEY_0, onKeyDown = {if(my_numbers(0).number == 0) my_numbers(0).number = 1 else my_numbers(0).number = 0})
  key(KEY_1, onKeyDown = {if(my_numbers(1).number == 0) my_numbers(1).number = 1 else my_numbers(1).number = 0})
  key(KEY_2, onKeyDown = {if(my_numbers(2).number == 0) my_numbers(2).number = 1 else my_numbers(2).number = 0})
  key(KEY_3, onKeyDown = {if(my_numbers(3).number == 0) my_numbers(3).number = 1 else my_numbers(3).number = 0})
  key(KEY_4, onKeyDown = {if(my_numbers(4).number == 0) my_numbers(4).number = 1 else my_numbers(4).number = 0})
  key(KEY_5, onKeyDown = {if(my_numbers(5).number == 0) my_numbers(5).number = 1 else my_numbers(5).number = 0})
  key(KEY_6, onKeyDown = {if(my_numbers(6).number == 0) my_numbers(6).number = 1 else my_numbers(6).number = 0})
  key(KEY_7, onKeyDown = {if(my_numbers(7).number == 0) my_numbers(7).number = 1 else my_numbers(7).number = 0})
  key(KEY_8, onKeyDown = {if(my_numbers(8).number == 0) my_numbers(8).number = 1 else my_numbers(8).number = 0})
  key(KEY_9, onKeyDown = {if(my_numbers(9).number == 0) my_numbers(9).number = 1 else my_numbers(9).number = 0})

  dispose {
    NetServer.stopServer()
  }
}

object MyServerCircle extends OutObject {
  import NetworkSynchServer._

  val networkId = "circle"
  val objectType = "circle"

  private var _coord = windowCenter
  def coord = _coord

  private var dir = Vec.zero
  key(KEY_W, 10, onKeyDown = {dir += Vec( 0,  1)})
  key(KEY_A, 10, onKeyDown = {dir += Vec(-1,  0)})
  key(KEY_S, 10, onKeyDown = {dir += Vec( 0, -1)})
  key(KEY_D, 10, onKeyDown = {dir += Vec( 1,  0)})

  action {
    if(dir != Vec.zero) {
      _coord += dir.n
      dir = Vec.zero
      is_changed = true
    }
  }

  render {
    drawCircle(_coord, 10, WHITE)
  }

  def state = State("coord" -> _coord)

  private var is_changed = false
  def isChanged = is_changed
  def changesSent() {is_changed = false}
}

trait InObject {
  def networkId:String
  def update(new_state:State)
}

trait InList[A <: InObject] extends InObject {
  this: Seq[A] =>
  def networkId:String
  def update(s:State) {
    s.neededKeys {
      case ("list", list:List[State]) => update(list)
    }
  }
  def update(ls:List[State]) {
    ls.foreach(elem => elem match {
      case State(("network_id", network_id:String),
                 ("position", position:Float),
                 ("state", state:State)) =>
        if(position >= 0 && position < length) {
          this(position.toInt).update(state)
        }
    })
  }
}

class MyClientNumber(val networkId:String) extends InObject {
  private var _number = 0
  def number = _number

  def update(s:State) {
    s.neededKeys {
      case ("number", new_number:Float) =>
        _number = new_number.toInt
    }
  }
}

object NetworkSynchClient extends ScageScreenApp("Network Synch Client", 640, 480) {
  private val in_objects = mutable.HashMap[String, InObject]()
  private val object_builders = mutable.HashMap[String, () => InObject]()
  private val in_lists = mutable.HashMap[String, InList[_]]()

  private val my_numbers = new ArrayBuffer[MyClientNumber] with InList[MyClientNumber] {
    val networkId = "my_numbers"
  }
  my_numbers ++= (0 to 9).map(i => new MyClientNumber(nextId.toString))

  in_lists += (my_numbers.networkId -> my_numbers)

  //in_objects += (MyClientCircle.networkId -> MyClientCircle)
  object_builders += ("circle" -> (() => MyClientCircle))

  NetClient.startClient(
    onServerDataReceived = {data =>
      data.neededKeys {
        case ("delta", out_objects:List[State]) => out_objects.foreach(out_object => out_object match {
          case State(("list", list_elem:List[State]),
                     ("network_id", network_id:String)) =>
            in_lists.get(network_id) match {
              case Some(in_list) => in_list.update(list_elem)
              case None =>
            }
          case State(("object", obj @ State(("network_id", network_id:String),
                                            ("remove", true)))) =>
            in_objects -= network_id
          case State(("object", obj @ State(("network_id", network_id:String),
                                            ("object_type", object_type:String),
                                            ("state", state:State)))) =>
            in_objects.get(network_id) match {
              case Some(in_obj) =>
                in_obj.update(state)
              case None =>
                object_builders.get(object_type) match {
                  case Some(build_func) =>
                    val in_obj = build_func()
                    in_objects += (in_obj.networkId -> in_obj)
                    in_obj.update(state)
                  case None =>
                }
            }
        })
      }
    }
  )

  backgroundColor = BLACK
  render {
    val str = my_numbers.map(_.number).mkString(" ")
    printCentered(str, windowCenter, WHITE)
  }

  dispose {
    NetClient.stopClient()
  }
}

object MyClientCircle extends InObject {
  import NetworkSynchClient._

  val networkId = "circle"

  private var _coord = windowCenter
  def coord = _coord

  def update(s:State) {
    s.neededKeys {
      case ("coord", new_coord:Vec) => _coord = new_coord
    }
  }

  render {
    drawCircle(_coord, 10, WHITE)
  }
}


