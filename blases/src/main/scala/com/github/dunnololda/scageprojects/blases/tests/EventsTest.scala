package com.github.dunnololda.scageprojects.blases.tests

object EventsTest extends ScageApp("Events Test") {
  /*private val my_events = new HashMap[String, ArrayBuffer[PartialFunction[Any, Unit]]]()
  
  def onMyEvent(event_name:String)(event_action: PartialFunction[Any, Unit]) {
    if(my_events.contains(event_name)) my_events(event_name) += event_action
    else my_events += (event_name -> ArrayBuffer(event_action))
  }
  
  def callMyEvent(event_name:String, arg:Any) {
    my_events.get(event_name) match {
      case Some(events_for_name) => events_for_name.foreach(_(arg))
      case None => println("event "+event_name+" not found")
    }
  }*/

  val foo = new Foo
  val bar = new Bar
}

class Foo {
  onEventWithArguments("I Bar") {
    case i:Int =>
      if(i > 0) {
        println("I heard it already for "+i+" times!")
        callEvent("I am Foo", (100, 500))
      }
  }
}

class Bar {
  private var i = 0
  action(1000) {
    println("I Bar!")
    callEvent("I Bar", i)
    i += 1
  }

  onEventWithArguments("I am Foo") {
    case (i:Int, j:Int) =>
      println("You Foo! "+i+" "+j)
    case s:String => println(s)
  }
}
