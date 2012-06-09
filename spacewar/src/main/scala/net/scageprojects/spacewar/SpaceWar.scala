package net.scageprojects.spacewar

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.{Vec, ScageColor}
import collection.mutable.ArrayBuffer

sealed class GameState

object PlayerWin extends GameState
object EnemyWin extends GameState
object NobodyWin extends GameState

object SpaceWar extends ScageScreenApp("Space War", 640, 480){
  private val planets = ArrayBuffer[Planet]()
  clear {
    planets.clear()
  }

  def addPlanet(size:Int = 10 + (math.random*3).toInt*10, ships:Int = (math.random*10).toInt, commander:Commander = Commander(), tries:Int = 5) {
    val new_coord = Vec(30 + math.random*((windowWidth - 30) - 30), 30 + math.random*((windowHeight - 30) - 30))
    if(planets.forall(p => p.coord.dist(new_coord) >= p.size + size + 100)) {
      planets += new Planet(new_coord, size, ships, commander)
    }
    else if(tries > 0) addPlanet(size, ships, commander, tries - 1)
    else println("failed to add new planet: size="+size+" commander="+commander)
  }

  val player = Commander("player", CYAN)
  val enemy = Commander("enemy", RED)

  init {
    addPlanet(size = 30, commander = player)
    addPlanet(size = 30, commander = enemy)
    for(i <- 1 to 10) addPlanet()
  }

  // enemy AI
  // attack phase: find nearest planet and send a fleet to it
  // defence phase: find some planet, player is nearest to and send a fleet to it
  action(1000) {
    val (enemy_planets, nonenemy_planets) = planets.partition(_.commander == enemy)
    val attackers_and_targets = for {
      enemy_planet <- enemy_planets
      nonenemy_planet <- nonenemy_planets
      if enemy_planet.ships > nonenemy_planet.ships
    } yield (enemy_planet, nonenemy_planet)
    if(attackers_and_targets.nonEmpty) {
      val (attacker, target) = attackers_and_targets.sortWith {
        case ((e1, n1), (e2, n2)) => e1.coord.dist(n1.coord) >= e2.coord.dist(n2.coord)
      }.head

      attacker.sendFleet(target)
    }
  }

  private var game_state:GameState = NobodyWin
  init {
    game_state = NobodyWin
  }

  action(1000) {
    game_state =
      if(     planets.forall(_.commander != enemy))  PlayerWin
      else if(planets.forall(_.commander != player)) EnemyWin
      else                                           NobodyWin
    if(game_state != NobodyWin) pause()
  }

  interface {
    game_state match {
      case PlayerWin => printCentered("You Win! Press space to restart",  windowCenter, CYAN)
      case EnemyWin  => printCentered("You lose. Press space to restart", windowCenter, CYAN)
      case _ =>
    }
  }

  keyNoPause(KEY_SPACE, onKeyDown = {if(onPause) restart(); pauseOff()})

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
            planet.sendFleet(other_planet)
          case None =>
        }
        selected_planet = None
      case None =>
        selected_planet = planets.find(p => p.coord.dist(m) < p.size && p.commander.name == player.name)
    }
  })

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
      (_ships - other_fleet.amount) match {
        case i if i >  0 => _ships = i
        case i if i <  0 => _ships = -i; _commander = other_fleet.commander   // maybe memory leak?
        case i if i == 0 => _ships = 0;  _commander = Commander()
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

  private val render_id = render {
    drawCircle(coord, size, _commander.color)
    printCentered(_ships, coord, _commander.color)
  }

  clear {
    delOperations(action_id, render_id, currentOperation)
  }
}

class SpaceFlight(fleet:Fleet, from_planet:Planet, to_planet:Planet) {
  val dir = (to_planet.coord - from_planet.coord).n
  val flight_speed = 50f
  private var current_coord = from_planet.coord + dir*from_planet.size

  private val action_id = action(10) {
    if(current_coord.dist(to_planet.coord) < to_planet.size) {
      to_planet.fleetArrival(fleet)
      deleteSelf()
    } else {
      current_coord += (dir*flight_speed/100)
    }
  }

  val dashed_line = dashedLine(from_planet.coord + dir*from_planet.size, to_planet.coord - dir*to_planet.size, 10)
  private val render_id = render {
    if(current_coord.dist(to_planet.coord) < to_planet.size) deleteSelf()
    else {
      drawLines(dashed_line, fleet.commander.color)
      drawPolygon(Vec(current_coord + 5*dir), current_coord - 10*dir.rotateDeg(20), current_coord - 10*dir.rotateDeg(-20))    // drawShip
      print(fleet.amount, current_coord)
    }
  }

  clear {
    if(operationExists(action_id)) delOperation(action_id)
    if(operationExists(render_id)) delOperation(render_id)
    deleteSelf()
  }
}
