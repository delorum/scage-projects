package com.github.dunnololda.scageprojects.spacewar

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer
import collection.mutable

sealed class GameState

object PlayerWin extends GameState
object EnemyWin extends GameState
object NobodyWin extends GameState

object SpaceWar extends ScageScreenApp("Space War", 800, 600) {
  private val planets = ArrayBuffer[Planet]()
  def addPlanet(size:Int = 10 + (math.random*3).toInt*10, ships:Int = (math.random*10).toInt, commander:Commander = Commander(), tries:Int = 5) {
    val new_coord = Vec(30 + math.random*((windowWidth - 30) - 30), 30 + math.random*((windowHeight - 30) - 30))
    if(planets.forall(p => p.coord.dist(new_coord) >= p.size + size + 100)) {
      planets += new Planet(new_coord, size, ships, commander)
    }
    else if(tries > 0) addPlanet(size, ships, commander, tries - 1)
    else println("failed to add new planet: size="+size+" commander="+commander)
  }

  private val routes = mutable.HashMap[Planet, ArrayBuffer[Planet]]()
  def addRoute(from_planet:Planet, to_planet:Planet) {
    if(!routeExists(from_planet, to_planet)) {
      routesFrom(from_planet) += to_planet
      routesFrom(to_planet) += from_planet
      new SpaceRoute(from_planet, to_planet)
    }
  }
  def routesFrom(planet:Planet) = routes.getOrElseUpdate(planet, ArrayBuffer[Planet]())
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

  clear {
    planets.clear()
    routes.clear()
    space_flights_from_planet.clear()
    space_flights_to_planet.clear()
  }

  val nobody = Commander()
  val player = Commander("player", CYAN)
  val enemy  = Commander("enemy",  RED)

  // creating universe
  init {
    addPlanet(size = 30, commander = player)
    addPlanet(size = 30, commander = enemy)     // may fail to add planet due to restrictions, need to do smth in that case
    for(i <- 1 to 10) addPlanet()

    for {
      planet <- planets
      nearest_planets = planets.filter(_ != planet).sortWith {
        case (p1, p2) => p1.coord.dist(planet.coord) < p2.coord.dist(planet.coord)
      }.take(1 + (math.random*3).toInt)
    } {
      nearest_planets.foreach(other_planet => addRoute(planet, other_planet))   // PROBLEM: may create separated clusters without routes between!!!
    }
  }

  // enemy AI
  // attack phase: find nearest planet and send a fleet to it
  // defence phase: find some planet, player is nearest to and send a fleet to it
  actionStaticPeriod(1000) {
    val enemy_planets = planets.filter(_.commander == enemy)
    (msecsFromInit / 1000) % 3 match {
      case 0 =>
        // attack phase
        // find nearest nonenemy planet and send a fleet to it
        val attackers_and_targets = for {
          enemy_planet <- enemy_planets
          nonenemy_planets = routesFrom(enemy_planet).filter(_.commander != enemy)
          nonenemy_planet <- nonenemy_planets
          if enemy_planet.ships > (nonenemy_planet.ships + flightsToFrom(enemy_planet, nonenemy_planet).foldLeft(0) {
            case (ships_amount, flight) => ships_amount + flight.fleet.amount
          }) && (!flightExists(enemy_planet, nonenemy_planet) || nonenemy_planet.commander != nobody)
        } yield (enemy_planet, nonenemy_planet)
        if(attackers_and_targets.nonEmpty) {
          val (attacker, target) = attackers_and_targets.sortWith {
            case ((e1, n1), (e2, n2)) => e1.coord.dist(n1.coord) < e2.coord.dist(n2.coord)
          }.head
          attacker.sendFleet(target)
        }
      case 1 =>
        // defence phase 1
        // find inner planets connected to other inner planets and send some fleet
        val pew1 = for {
          enemy_planet <- enemy_planets
          if enemy_planet.ships > 0
          connected_planets = routesFrom(enemy_planet)
          if connected_planets.forall(_.commander == enemy)
          other_inner_planets = connected_planets.filter(p => routesFrom(p).exists(_.commander == enemy))
          if other_inner_planets.nonEmpty
        } yield (enemy_planet, other_inner_planets)
        if(pew1.nonEmpty) {
          val (p1, p2) = pew1((math.random*pew1.length).toInt)
          p1.sendFleet(p2((math.random*p2.length).toInt))
        }
      case 2 =>
        // defence phase 2
        // find inner planets connected to outer planets and send some fleet
        val pew2 = for {
          enemy_planet <- enemy_planets
          if enemy_planet.ships > 0
          connected_planets = routesFrom(enemy_planet)
          if connected_planets.forall(_.commander == enemy)
          outer_planets = connected_planets.filter(p => routesFrom(p).exists(_.commander != enemy))
          if outer_planets.nonEmpty
        } yield (enemy_planet, outer_planets)
        if(pew2.nonEmpty) {
          val (p1, p2) = pew2((math.random*pew2.length).toInt)
          p1.sendFleet(p2((math.random*p2.length).toInt))
        }
      case _ =>
    }
  }

  private var game_state:GameState = NobodyWin
  init {
    game_state = NobodyWin
  }

  actionStaticPeriod(1000) {
    game_state =
      if(     planets.forall(_.commander != enemy)  && space_flights_from_planet.values.flatten.forall(_.fleet.commander != enemy))  PlayerWin
      else if(planets.forall(_.commander != player) && space_flights_from_planet.values.flatten.forall(_.fleet.commander != player)) EnemyWin
      else                                                                                                                           NobodyWin
    if(game_state != NobodyWin) pause()
  }

  interface {
    if(onPause) game_state match {
      case PlayerWin => printCentered("You Win! Press F2 to restart",   windowCenter, CYAN)
      case EnemyWin  => printCentered("You lose. Press F2 to restart",  windowCenter, RED)
      case _ =>         printCentered("PAUSE. Press space to continue", windowCenter, CYAN)
    }
  }

  keyIgnorePause(KEY_SPACE, onKeyDown = switchPause())
  keyIgnorePause(KEY_F2, onKeyDown = {restart(); pauseOff()})
  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp())

  private var selected_planet:Option[Planet] = None
  init {
    selected_planet = None
  }

  render {
    selected_planet match {
      case Some(planet) => drawCircle(planet.coord, planet.size + planet.size*0.2f, planet.commander.color)
      case None =>
    }
  }

  leftMouse(onBtnDown = {m =>
    selected_planet match {
      case Some(planet) =>
        planets.find(p => p.coord.dist(m) < p.size) match {
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
  })
}

case class Commander(name:String = "nobody", color:ScageColor = GRAY)

case class Fleet(var amount:Int, commander:Commander)

import SpaceWar._

// size is one of 10, 20, 30
class Planet(val coord:Vec, val size:Int = 10 + (math.random*3).toInt*10, init_ships:Int = (math.random*10).toInt, init_commander:Commander = Commander()) {
  private var _ships     = init_ships
  def ships = _ships

  private var _commander = init_commander
  def commander = _commander

  def fleetArrival(other_fleet:Fleet) {
    if(_commander != other_fleet.commander) {
      _ships - other_fleet.amount match {
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

  private val action_id = actionStaticPeriod(production_period) {
    if(_commander.name != "nobody") _ships += 1
  }

  private val render_id = render {
    drawCircle(coord, size, _commander.color)
    printCentered(_ships, coord, _commander.color)
  }

  clear {
    delOperations(action_id, render_id, currentOperation)
  }
}

class SpaceRoute(from_planet:Planet, to_planet:Planet) {
  val dir = (to_planet.coord - from_planet.coord).n
  val dashed_line = dashedLine(from_planet.coord + dir*from_planet.size, to_planet.coord - dir*to_planet.size, 10)
  private val render_id = render {
    val route_color = if(from_planet.commander.name == to_planet.commander.name) from_planet.commander.color else GRAY
    drawLines(dashed_line, route_color)
  }

  clear {
    if(operationExists(render_id)) delOperation(render_id)
    deleteSelf()
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

class SpaceFlight(val fleet:Fleet, val from_planet:Planet, val to_planet:Planet) {
  addFlight(this)

  val dir = (to_planet.coord - from_planet.coord).n
  val flight_speed = 50f
  private var current_coord = from_planet.coord + dir*from_planet.size
  def currentCoord = current_coord

  private val action_id = actionStaticPeriod(10) {
    if(current_coord.dist(to_planet.coord) < to_planet.size) {
      to_planet.fleetArrival(fleet)
      remove()
    } else {
      flightsToFrom(from_planet, to_planet).find(sp => sp.fleet.commander != fleet.commander && sp.currentCoord.dist(current_coord) < 5) match {
        case Some(enemy_space_flight) =>
          fleet.amount - enemy_space_flight.fleet.amount match {
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

  private val render_id = render {
    currentColor = fleet.commander.color
    drawPolygon(Vec(current_coord + 5*dir), current_coord - 10*dir.rotateDeg(20), current_coord - 10*dir.rotateDeg(-20))    // drawShip
    print(fleet.amount, current_coord)
  }

  clear {
    remove()
    deleteSelf()
  }

  def remove() {
    delOperationsNoWarn(action_id, render_id)
    removeFlight(this)
  }
}
