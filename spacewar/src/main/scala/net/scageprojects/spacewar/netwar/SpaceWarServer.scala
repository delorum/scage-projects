package net.scageprojects.spacewar.netwar

import net.scage.ScageApp
import net.scage.support.net.NetServer
import net.scage.ScageLib._
import net.scage.support.{Vec, ScageColor, State}
import collection.mutable.{ListBuffer, HashMap, ArrayBuffer}
import collection.mutable

object SpaceWarServer extends ScageApp("Space War Server") {
  val window_width = 800
  val window_height = 600

  private val planets = ListBuffer[Planet]()
  private def addPlanet(size:Int = 10 + (math.random*3).toInt*10, ships:Int = (math.random*10).toInt, commander:Commander = Commander(), tries:Int = 5) {
    val new_coord = Vec(30 + math.random*((window_width - 30) - 30), 30 + math.random*((window_height - 30) - 30))
    if(planets.forall(p => p.coord.dist(new_coord) >= p.size + size + 100)) {
      planets += new Planet(new_coord, size, ships, commander)
    }
    else if(tries > 0) addPlanet(size, ships, commander, tries - 1)
    else println("failed to add new planet: size="+size+" commander="+commander)
  }

  private val routes_from_planet = mutable.HashMap[Planet, ArrayBuffer[Planet]]()
  def addRoute(from_planet:Planet, to_planet:Planet) {
    if(!routeExists(from_planet, to_planet)) {
      routesFrom(from_planet) += to_planet
      routesFrom(to_planet) += from_planet
    }
  }
  def routesFrom(planet:Planet) = routes_from_planet.getOrElseUpdate(planet, ArrayBuffer[Planet]())
  def routeExists(from_planet:Planet, to_planet:Planet) = routesFrom(from_planet).contains(to_planet)

  private val space_flights_from_planet = mutable.HashMap[Planet, ArrayBuffer[SpaceFlight]]()
  def addFlight(space_flight:SpaceFlight) {
    flightsFrom(space_flight.from_planet) += space_flight
    flightsTo(space_flight.to_planet)     += space_flight
  }
  def removeFlight(space_flight:SpaceFlight) {
    flightsFrom(space_flight.from_planet) -= space_flight
    flightsTo(space_flight.to_planet)     -= space_flight
  }
  def flightsFrom(planet:Planet) = space_flights_from_planet.getOrElseUpdate(planet, ArrayBuffer[SpaceFlight]())
  def flightsFromTo(from_planet:Planet, to_planet:Planet) = flightsFrom(from_planet).filter(_.to_planet == to_planet)
  def flightExists(from_planet:Planet, to_planet:Planet) = flightsFrom(from_planet).exists(_.to_planet == to_planet)

  private val space_flights_to_planet = mutable.HashMap[Planet, ArrayBuffer[SpaceFlight]]()
  def flightsTo(planet:Planet) = space_flights_to_planet.getOrElseUpdate(planet, ArrayBuffer[SpaceFlight]())
  def flightsToFrom(to_planet:Planet, from_planet:Planet) = flightsTo(to_planet).filter(_.from_planet == from_planet)

  private val players = ArrayBuffer[Commander]()
  private val player_colors = List(CYAN, RED, YELLOW, GREEN).iterator

  NetServer.startServer(
    max_clients = 4,
    onClientAccepted = {client =>
      players += Commander(client.id.toString, player_colors.next())
      if(!countdown_started && players.length > 1) {
        startCountDown()
      }
    }
  )

  import concurrent.ops._
  spawn {
    while(msecsFromInit < 10000) {}
    startCountDown()
  }

  private var countdown_started = false
  def startCountDown() = {
    countdown_started = true
    val start_moment = msecs
    action(1000) {
      println("pew")
      val current_moment = msecsFrom(start_moment)/1000
      NetServer.sendToAll(State("countdown" -> current_moment))
      if(current_moment >= 10) {
        deleteSelf()
        startGameRound()
      }
    }
  }

  def startGameRound() {
    players.foreach(pl => addPlanet(size = 30, commander = pl))
    for(i <- 1 to 10) addPlanet()
    for {
      planet <- planets
      nearest_planets = planets.filter(_ != planet).sortWith {
        case (p1, p2) => p1.coord.dist(planet.coord) < p2.coord.dist(planet.coord)
      }.take(1 + (math.random*3).toInt)
    } {
      nearest_planets.foreach(other_planet => addRoute(planet, other_planet))   // PROBLEM: may create separated clusters without routes between!!!
    }

    NetServer.sendToAll(State("planets" -> planets.map(_.state).toList))
    NetServer.sendToAll(State("routes" -> routes_from_planet.map{
      case (from, to_list) => to_list.map(to => State("from" -> from.state, "to" -> to.state))
    }.flatten.toList))

    action(10) {
      NetServer.sendToAll(State("planets" -> planets.map(_.state).toList))
      NetServer.sendToAll(State("flights" -> space_flights_from_planet.values.flatten.map(_.state).toList))
    }
    //NetServer.stopServer()
  }
}

import SpaceWarServer._

case class Commander(name:String = "nobody", color:ScageColor = GRAY) {
  def state = State("name" -> name, "color" -> color)
}

case class Fleet(var amount:Int, commander:Commander) {
  def state = State("amount" -> amount, "commander" -> commander.state)
}

class Planet(val coord:Vec, val size:Int = 10 + (math.random*3).toInt*10, init_ships:Int = (math.random*10).toInt, init_commander:Commander = Commander()) {
  private var _ships     = init_ships
  def ships = _ships

  def state = State("ships" -> ships, "commander" -> commander.state, "coord" -> coord, "size" -> size)

  private var _commander = init_commander
  def commander = _commander

  def fleetArrival(other_fleet:Fleet) {
    if(_commander != other_fleet.commander) {
      (_ships - other_fleet.amount) match {
        case i if i >=  0 => _ships = i
        case i if i <  0 => _ships = -i; _commander = other_fleet.commander   // maybe memory leak?
      }
    } else _ships += other_fleet.amount
  }

  def sendFleet(to_planet:Planet) {
    new SpaceFlight(Fleet(_ships, _commander), this, to_planet)
    _ships = 0
  }

  val production_period = size match {
    case 10 => 3000
    case 20 => 2000
    case 30 => 1000
    case _  => 3000
  }

  private val action_id = action(production_period) {
    if(_commander.name != "nobody") _ships += 1
  }

  clear {
    delOperations(action_id, currentOperation)
  }
}

class SpaceFlight(val fleet:Fleet, val from_planet:Planet, val to_planet:Planet) {
  addFlight(this)

  val dir = (to_planet.coord - from_planet.coord).n
  val flight_speed = 50f
  private var current_coord = from_planet.coord + dir*from_planet.size
  def currentCoord = current_coord

  def state = State("coord" -> current_coord, "fleet" -> fleet.state, "dir" -> dir)

  private val action_id = action(10) {
    if(current_coord.dist(to_planet.coord) < to_planet.size) {
      to_planet.fleetArrival(fleet)
      remove()
    } else {
      flightsToFrom(from_planet, to_planet).find(sp => sp.fleet.commander != fleet.commander && sp.currentCoord.dist(current_coord) < 5) match {
        case Some(enemy_space_flight) =>
          (fleet.amount - enemy_space_flight.fleet.amount) match {
            case i if i > 0 =>
              fleet.amount = i
              enemy_space_flight.remove()
            case i if i < 0 =>
              remove()
              enemy_space_flight.fleet.amount = -i
            case i if i == 0 =>
              remove()
              enemy_space_flight.remove()
          }
        case None =>
      }
      current_coord += (dir*flight_speed/100)
    }
  }

  clear {
    remove()
    deleteSelf()
  }

  def remove() {
    delOperationsNoWarn(action_id)
    removeFlight(this)
  }
}
