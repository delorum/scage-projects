package net.scageprojects.spacewar.tests

import net.scage.support.State
import collection.generic.{Shrinkable, Growable}
import collection.mutable.ArrayBuffer
import net.scage.support.net.{NetClient, NetServer}
import net.scage.ScageLib._
import net.scage.{ScageScreenApp, ScageApp}
import collection.mutable

trait NetObject {
  def networkId:String
  def objectType:String
  def fullState:State
  def delta:State
  def update(s:State )
  def isChanged:Boolean
  def markChanged()
  def allChangesSent()
}

trait NetBuilder {
  def build[N <: NetObject](network_id:String):N
}

object NetBuilders {
  val builders = mutable.HashMap[String, NetBuilder]()
  builders += ("net_int" -> new NetBuilder {def build[N <: NetObject](network_id:String) = new NetInt(network_id).asInstanceOf[N]})
  builders += ("net_buffer" -> new NetBuilder {def build[N <: NetObject](network_id:String) = new NetBuffer[N](network_id).asInstanceOf[N]})
}

import NetBuilders._

object NetLayer2Test extends ScageApp("Having Pink Nightmares") {
  val net_objects = mutable.HashMap[String, NetObject]()

  NetServer.startServer(
    onClientAccepted = {client =>
      val object_states = net_objects.view.map(_._2).map(net_object => State(
        "network_id"  -> net_object.networkId,
        "object_type" -> net_object.objectType,
        "state"       -> net_object.fullState
      )).toList
      val data = State("net_objects" -> object_states)
      client.send(data)
    },
    onClientDataReceived = {(client, data) =>
      data.neededKeys {
        case ("net_objects", net_object_states:List[State]) => net_object_states.foreach(_ match {
          case State(("network_id",   network_id:String),
                     ("object_type",  object_type:String),
                     ("state", net_object_state:State)) =>
            val elem = net_objects.getOrElseUpdate(network_id, builders(object_type).build[NetObject](network_id))
            elem.update(net_object_state)
            elem.markChanged()
        })
      }
    }
  )

  action(30) {
    val changed_objects = net_objects.withFilter {
      case (network_id, net_object) => net_object.isChanged
    }.map(_._2)
    if(changed_objects.nonEmpty) {
      val changed_object_states = changed_objects.map(net_object => State(
        "network_id"  -> net_object.networkId,
        "object_type" -> net_object.objectType,
        "state"       -> net_object.delta
      )).toList
      val data = State("net_objects" -> changed_object_states)
      NetServer.sendToAll(data)
      changed_objects.foreach(_.allChangesSent())
    }
  }

  val buf = new NetBuffer[NetInt]("Keeping You Sane")
  net_objects += (buf.networkId -> buf)
}

object NetLayer2TestClient extends ScageScreenApp("Hugging Six Rabits", 640, 480) {
  val net_objects = mutable.HashMap[String, NetObject]()

  NetClient.startClient(
    onServerDataReceived = {data =>
      data.neededKeys {
        case ("net_objects", net_object_states:List[State]) => net_object_states.foreach(_ match {
          case State(("network_id",   network_id:String),
          ("object_type",  object_type:String),
          ("state", net_object_state:State)) =>
            val elem = net_objects.getOrElseUpdate(network_id, builders(object_type).build[NetObject](network_id))
            elem.update(net_object_state)
        })
      }
    }
  )
  action(30) {
    val changed_objects = net_objects.withFilter {
      case (network_id, net_object) => net_object.isChanged
    }.map(_._2)
    if(changed_objects.nonEmpty) {
      val changed_object_states = changed_objects.map(net_object => State(
        "network_id"  -> net_object.networkId,
        "object_type" -> net_object.objectType,
        "state"       -> net_object.delta
      )).toList
      val data = State("net_objects" -> changed_object_states)
      NetClient.send(data)
      changed_objects.foreach(_.allChangesSent())
    }
  }

  val buf = new NetBuffer[NetInt]("Keeping You Sane")
  net_objects += (buf.networkId -> buf)

  val my_number = new NetInt(nextId.toString, 0)
  buf += my_number

  key(KEY_0, onKeyDown = {my_number.value = 0})
  key(KEY_1, onKeyDown = {my_number.value = 1})
  key(KEY_2, onKeyDown = {my_number.value = 2})
  key(KEY_3, onKeyDown = {my_number.value = 3})
  key(KEY_4, onKeyDown = {my_number.value = 4})
  key(KEY_5, onKeyDown = {my_number.value = 5})
  key(KEY_6, onKeyDown = {my_number.value = 6})
  key(KEY_7, onKeyDown = {my_number.value = 7})
  key(KEY_8, onKeyDown = {my_number.value = 8})
  key(KEY_9, onKeyDown = {my_number.value = 9})

  backgroundColor = BLACK
  render {
    val str_buf = new StringBuilder
    buf.foreach(elem => {
      if(elem.networkId == my_number.networkId) str_buf.append("[r"+elem.value+"] ")
      else str_buf.append(elem.value+" ")
    })
    val str = str_buf.toString()
    printCentered(str, windowCenter, WHITE)
  }
}

class NetInt(val networkId:String, init_value:Int = 0) extends NetObject {
  val objectType = "net_int"

  private var _value = init_value
  def value =  _value

  private var is_changed = false
  def value_=(new_value:Int) {
    _value = new_value
    is_changed = true
  }

  def fullState:State = State("value" -> _value)
  def delta:State = State("value" -> _value)
  def update(s:State ) {
    s.neededKeys {
      case ("value", new_value:Float) => _value = new_value.toInt
    }
  }
  def markChanged() {is_changed = true}
  def isChanged:Boolean = is_changed
  def allChangesSent() {is_changed = false}
}

class NetBuffer[N <: NetObject](val networkId:String, init_arr:N*) extends NetObject with Seq[N] with Growable[N] with Shrinkable[N] {
  val objectType = "net_buffer"

  private val arr = ArrayBuffer(init_arr:_*)
  private var buffer_removes   = ArrayBuffer[N]()
  private var buffer_additions = ArrayBuffer[N]()

  def remove(idx:Int) = {
    buffer_removes += arr.remove(idx)
  }

  private var is_all_clear = false
  def clear() {
    arr.clear()
    is_all_clear = true
  }

  def +=(elem:N) = {
    arr += elem
    buffer_additions += elem
    this
  }

  def -=(elem:N) = {
    arr -= elem
    buffer_removes += elem
    this
  }

  def apply(idx: Int) = {arr.apply(idx)}
  def length: Int = {arr.length}
  def iterator: Iterator[N] = {arr.iterator}

  def fullState = {
    val to_return = State()
    val elems = view.map(elem => State("network_id"  -> elem.networkId,
                                       "object_type" -> elem.objectType,
                                       "state"       -> elem.fullState)).toList
    if(elems.nonEmpty)    to_return ++= State("elems" -> elems)
    if(is_all_clear)              to_return ++= State("clear")
    if(buffer_removes.nonEmpty)   to_return ++= State("remove" -> buffer_removes.map(elem => State("network_id"    -> elem.networkId)).toList)
    if(buffer_additions.nonEmpty) to_return ++= State("add"    -> buffer_additions.map(elem => State("network_id"  -> elem.networkId,
                                                                                                     "object_type" -> elem.objectType,
                                                                                                     "state"       -> elem.fullState)).toList)
    to_return
  }
  def delta = {
    val to_return = State()
    val changed_elems = view.filter(_.isChanged).map(elem => State("network_id"  -> elem.networkId,
                                                                   "object_type" -> elem.objectType,
                                                                   "state"       -> elem.delta)).toList
    if(changed_elems.nonEmpty)    to_return ++= State("elems" -> changed_elems)
    if(is_all_clear)              to_return ++= State("clear")
    if(buffer_removes.nonEmpty)   to_return ++= State("remove" -> buffer_removes.map(elem => State("network_id"    -> elem.networkId)).toList)
    if(buffer_additions.nonEmpty) to_return ++= State("add"    -> buffer_additions.map(elem => State("network_id"  -> elem.networkId,
                                                                                                     "object_type" -> elem.objectType,
                                                                                                     "state"       -> elem.delta)).toList)
    to_return
  }

  def update(s:State) {
    s.neededKeys {
      case ("add", new_elem_states:List[State]) =>
        new_elem_states.foreach(new_elem_state => {
          val network_id = new_elem_state.value[String]("network_id")
          val object_type = new_elem_state.value[String]("object_type")
          val new_elem = builders(object_type).build[N](network_id)
          val state = new_elem_state.value[State]("state")
          new_elem.update(state)
          arr += new_elem
        })
      case ("remove", remove_elem_states:List[State]) =>
        remove_elem_states.foreach(remove_elem_state => {
          val remove_index = arr.indexWhere(_.networkId == remove_elem_state.value[String]("network_id"))
          if(remove_index >= 0) arr.remove(remove_index)
        })
      case ("clear", true) => arr.clear()
      case ("elems", elem_states:List[State]) =>    // TODO: optimize this with additional map: network_id -> elem
        elem_states.foreach(elem_state => {
          val network_id = elem_state.value[String]("network_id")
          arr.find(_.networkId == network_id) match {
            case Some(elem) =>
              val state = elem_state.value[State]("state")
              elem.update(state)
            case None =>
              val object_type = elem_state.value[String]("object_type")
              val new_elem = builders(object_type).build[N](network_id)
              val state = elem_state.value[State]("state")
              new_elem.update(state)
              arr += new_elem
          }
        })
    }
  }

  def markChanged() {arr.foreach(_.markChanged())}

  def isChanged =
    is_all_clear              ||
    buffer_additions.nonEmpty ||
    buffer_removes.nonEmpty   ||
    arr.exists(_.isChanged)

  def allChangesSent() {
    is_all_clear = false
    buffer_additions.clear()
    buffer_removes.clear()
    arr.foreach(_.allChangesSent())

  }
}