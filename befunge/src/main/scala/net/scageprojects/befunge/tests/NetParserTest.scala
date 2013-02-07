package net.scageprojects.liftdriver.tests

import net.scage.ScageLib.{State => ScageState, _}
import actors.Actor._
import actors.TIMEOUT

case class TelnetMessage(msg:String = "")

object DataParser {
  def parseTelnetMessage(data:ScageState):TelnetMessage = {
    data match {
      case ScageState(("msg", msg:String)) =>
        TelnetMessage(msg)
      case _ =>
        TelnetMessage()
    }
  }
}

object NetParserTest extends ScageApp("Net Parser") {
  fromActors(10) {
    case TelnetMessage(msg) =>
      println("received: "+msg)
    case TIMEOUT =>
  }

  NetServer.startServer(
    port = 9000,
    onClientDataReceived = (client, data) => {
      DataParser.parseTelnetMessage(data) match {
        case tm @ TelnetMessage(msg) =>
          if(msg == "stop") stopApp()
          else {
            client.send(msg)
            main_loop ! tm
          }
      }
    }
  )

  dispose {
    NetServer.stopServer()
  }
}
