package net.scageprojects.spacewar.netwar

import net.scage.ScageScreenApp
import net.scage.support.net.NetClient
import net.scage.ScageLib._
import net.scage.support.{Vec, ScageColor, State}
import collection.mutable.HashMap
import collection.mutable

object SpaceWarClient extends ScageScreenApp("Space War Client", 800, 600) {
  private val client_planets = mutable.HashMap[Vec, ClientPlanet]()
  def clientPlanet(planet_coord:Vec, planet_size:Int, planet_ships:Int, commander_color:ScageColor) = {
    client_planets.getOrElseUpdate(planet_coord, new ClientPlanet(planet_coord, planet_size, planet_ships, commander_color))
  }
  private val client_routes = mutable.HashMap[(ClientPlanet, ClientPlanet), ClientSpaceRoute]()
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
                                                       ("name",  commander_name:String))),
                       ("coord",     planet_coord:Vec),
                       ("ships",     planet_ships:Float),
                       ("size",      planet_size:Float)) =>
              clientPlanet(planet_coord, planet_size.toInt, planet_ships.toInt, commander_color).update(planet_ships.toInt, commander_color)
          })
        case ("routes", server_routes:List[State]) =>
          server_routes.foreach(server_route => server_route match {
            case State(("from", State(("commander", from_commander @ State(("color", from_commander_color:ScageColor),
                                                                           ("name",      from_commander_name:String))),
                                      ("coord",     from_planet_coord:Vec),
                                      ("ships",     from_planet_ships:Float),
                                      ("size",      from_planet_size:Float))),
                       ("to", State(("commander", to_commander @ State(("color", to_commander_color:ScageColor),
                                                                       ("name",      to_commander_name:String))),
                                    ("coord",     to_planet_coord:Vec),
                                    ("ships",     to_planet_ships:Float),
                                    ("size",      to_planet_size:Float)))) =>
              val from_planet = clientPlanet(from_planet_coord, from_planet_size.toInt, from_planet_ships.toInt, from_commander_color)
              val to_planet = clientPlanet(to_planet_coord, to_planet_size.toInt, to_planet_ships.toInt, to_commander_color)
              client_routes.getOrElseUpdate((from_planet, to_planet), new ClientSpaceRoute(from_planet, to_planet))
          })
      }
    }
  )

  private var selected_planet:Option[ClientPlanet] = None

  render {
    selected_planet match {
      case Some(planet) => drawCircle(planet.coord, planet.size + planet.size*0.2f, planet.commanderColor)
      case None =>
    }
  }

  /*leftMouse(onBtnDown = {m =>
    selected_planet match {
      case Some(planet) =>
        client_planets.values.find(p => p.coord.dist(m) < p.size) match {
          case some_other_planet @ Some(other_planet) =>
            if(routeExists(planet, other_planet)) {
              planet.sendFleet(other_planet)
              selected_planet = None
            } else if(other_planet.commander == player) selected_planet = some_other_planet
            else selected_planet = None
          case None => selected_planet = None
        }
      case None => selected_planet = planets.find(p => p.coord.dist(m) < p.size && p.commander.name == player.name)
    }
  })*/
}

import SpaceWarClient._

class ClientPlanet(val planet_coord:Vec, val planet_size:Int, private var planet_ships:Int, private var commander_color:ScageColor) {
class ClientPlanet(val coord:Vec, val size:Int, private var planet_ships:Int, private var commander_color:ScageColor) {
  def commanderColor = commander_color

  def update(new_planet_ships:Int, new_commander_color:ScageColor) {
    planet_ships = new_planet_ships
    commander_color = new_commander_color
  }

  private val render_id = render {
    drawCircle(coord, size, commander_color)
    printCentered(planet_ships, coord, commander_color)
  }

  clear {
    delOperations(render_id, currentOperation)
  }
}

class ClientSpaceRoute(val from_planet:ClientPlanet, val to_planet:ClientPlanet) {
  val dir = (to_planet.coord - from_planet.coord).n
  val dashed_line = dashedLine(from_planet.coord + dir*from_planet.size, to_planet.coord - dir*to_planet.size, 10)
  private val render_id = render {
    val route_color = if(from_planet.commanderColor == to_planet.commanderColor) from_planet.commanderColor else GRAY
    drawLines(dashed_line, route_color)
  }

  clear {
    delOperations(render_id, currentOperation)
  }

  def dashedLine(from:Vec, to:Vec, dash_size:Int) = {
    val dir = (to - from).n
    val dashes = math.round((to - from).norma/dash_size/2)
    val pew = for {
      i <- 0 until dashes
      start = from + (i*dash_size*2)*dir
      stop  = from + (i*dash_size*2 + dash_size)*dir
    } yield (start, stop)
    pew.flatMap {
      case (start, stop) => Seq(start, stop)
    }
  }
}
