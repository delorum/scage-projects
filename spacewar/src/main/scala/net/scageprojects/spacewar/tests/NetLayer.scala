package net.scageprojects.spacewar.tests

import collection.mutable
import net.scage.support.State

class NetLayer {
  private val out_objects = mutable.HashMap[String, OutObject]()

  def outAddObject(obj:OutObject) {out_objects += (obj.networkId -> obj)}

  def outFull = out_objects.values.map(elem => State("object" -> State("network_id"  -> elem.networkId,
                                                                       "object_type" -> elem.objectType,
                                                                       "state"       -> elem.state)))

  def outDelta = out_objects.values.filter(_.isChanged).map(elem => State("object" -> State("network_id"  -> elem.networkId,
                                                                                            "object_type" -> elem.objectType,
                                                                                            "state"       -> elem.state)))

  def outChangesSent() {out_objects.values.filter(_.isChanged).foreach(_.changesSent())}

  private val in_objects = mutable.HashMap[String, InObject]()
  private val in_object_builders = mutable.HashMap[String, String => InObject]()

  def inAddBuilder(object_type:String, build_func: String => InObject) {in_object_builders += (object_type -> build_func)}
  def inUpdate(data:State) {
    data.neededKeys {
      case ("delta", out_objects:List[State]) => out_objects.foreach(out_object => out_object match {
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
              in_object_builders.get(object_type) match {
                case Some(build_func) =>
                  val in_obj = build_func(network_id)
                  in_objects += (in_obj.networkId -> in_obj)
                  in_obj.update(state)
                case None =>
              }
          }
      })
    }
  }
}
