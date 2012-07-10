package net.scageprojects.spacewar.netwar

import net.scage.ScageScreenApp
import net.scage.support.net.NetClient
import net.scage.ScageLib._
import net.scage.support.{Vec, ScageColor, State}
import collection.mutable.HashMap

object SpaceWarClient extends ScageScreenApp("Space War Client", 800, 600) {
  val client_planets = HashMap[Vec, ClientPlanet]()
  NetClient.startClient(
    onServerDataReceived = {data =>
      data.neededKeys {
        case ("planets", server_planets:List[State]) =>
          server_planets.foreach(server_planet => server_planet match {
            case State(("commander", commander @ State(("color", commander_color:ScageColor),
                                                       ("name", commander_name:String))),
                       ("coord", planet_coord:Vec),
                       ("ships", planet_ships:Float),
                       ("size", planet_size:Float)) =>
              client_planets.getOrElseUpdate(planet_coord, new ClientPlanet(planet_coord, planet_size.toInt, planet_ships.toInt, commander_color)).update(planet_ships.toInt, commander_color)
          })
      }
    }
  )
}

import SpaceWarClient._

class ClientPlanet(val planet_coord:Vec, val planet_size:Int, private var planet_ships:Int, private var commander_color:ScageColor) {
  def update(new_planet_ships:Int, new_commander_color:ScageColor) {
    planet_ships = new_planet_ships
    commander_color = new_commander_color
  }

  private val render_id = render {
    drawCircle(planet_coord, planet_size, commander_color)
    printCentered(planet_ships, planet_coord, commander_color)
  }

  clear {
    delOperations(render_id, currentOperation)
  }
}
