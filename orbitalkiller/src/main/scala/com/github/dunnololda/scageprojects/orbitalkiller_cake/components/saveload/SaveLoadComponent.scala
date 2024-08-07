package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.saveload

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.main_screen.MainScreenAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder.ShipsHolderAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.ParseUtils.{
  safeParseDVec,
  safeParseDouble,
  safeParseInt,
  safeParseLong
}

import java.io.FileOutputStream

abstract class SaveLoadComponent
  extends MainScreenAware
  with SystemEvolutionAware
  with CelestialsAware
  with ShipsHolderAware {
  private var _show_game_saved_message = false

  def showGameSavedMessage: Boolean = _show_game_saved_message

  def saveGame(): Unit = {
    val fos = new FileOutputStream("save.orbitalkiller")
    fos.write(s"time ${systemEvolution.tacts}\n".getBytes)
    celestialsHelper.planets.values.foreach { celestial =>
      systemEvolution.bodyState(celestial.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    }
    shipsHolder.ships.foreach { ship =>
      systemEvolution.bodyState(ship.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    }
    fos.close()
    saveSuccess()
  }

  private var _show_game_loaded_message = false

  def showGameLoadedMessage: Boolean = _show_game_loaded_message

  private var _show_game_failed_to_load_message = false

  def showGameFailedToLoadMessage: Boolean = _show_game_failed_to_load_message

  def loadGame(): Unit = {
    val savefile_lines: List[String] = {
      val source = io.Source.fromFile("save.orbitalkiller")
      try {
        source.getLines().toList
      } finally {
        source.close()
      }
    }
    val optNewTacts = parseNewTacts(savefile_lines)

    val new_states: Map[Int, NewState] = parseNewStates(savefile_lines)

    if (loadNewTacts(optNewTacts) && loadCelestials(new_states) && loadShips(new_states)) {
      loadSuccess()
    } else {
      loadFailed()
    }
  }

  private def loadState(currentState: MutableBodyState, new_state: NewState): Unit = {
    val NewState(acc, vel, coord, ang_acc, ang_vel, ang) = new_state
    currentState.acc = acc
    currentState.vel = vel
    currentState.coord = coord
    currentState.ang_acc = ang_acc
    currentState.ang_vel = ang_vel
    currentState.ang = ang
  }

  private def parseNewStates(savefile_lines: List[String]): Map[Int, NewState] = {
    (for {
      line <- savefile_lines.drop(1)
      s = line.split(" ")
      if s.length == 7
      index <- safeParseInt(s(0))
      acc <- s(1).split("=", 2).lastOption.flatMap(safeParseDVec)
      vel <- s(2).split("=", 2).lastOption.flatMap(safeParseDVec)
      coord <- s(3).split("=", 2).lastOption.flatMap(safeParseDVec)
      ang_acc <- s(4).split("=", 2).lastOption.flatMap(safeParseDouble)
      ang_vel <- s(5).split("=", 2).lastOption.flatMap(safeParseDouble)
      ang <- s(6).split("=", 2).lastOption.flatMap(safeParseDouble)
    } yield {
      (index, NewState(acc, vel, coord, ang_acc, ang_vel, ang))
    }).toMap
  }

  private def parseNewTacts(savefile_lines: List[String]) = {
    savefile_lines.headOption.flatMap(l => {
      val s = l.split(" ")
      if (s.length == 2 && s.head == "time") safeParseLong(s.last) else None
    })
  }

  private def loadNewTacts(optNewTacts: Option[Long]): Boolean = {
    optNewTacts match {
      case Some(newTacts) =>
        systemEvolution.setTacts(newTacts)
        true
      case None =>
        false
    }
  }

  private def loadCelestials(new_states: Map[Int, NewState]): Boolean = {
    celestialsHelper.planets.values.foldLeft(true) { case (result, nextCelestialBody) =>
      if (!result) result
      else {
        new_states.get(nextCelestialBody.index) match {
          case Some(newState) =>
            loadState(nextCelestialBody.currentState, newState)
            result
          case None =>
            false
        }
      }
    }
  }

  private def loadShips(new_states: Map[Int, NewState]): Boolean = {
    shipsHolder.ships.foldLeft(true) { case (result, nextShip) =>
      if (!result) result
      else {
        new_states.get(nextShip.index) match {
          case Some(newState) =>
            loadState(nextShip.currentState, newState)
            result
          case None =>
            false
        }
      }
    }
  }

  private def saveSuccess(): Unit = {
    _show_game_saved_message = true
    val start = System.currentTimeMillis()
    mainScreen.actionStaticPeriodIgnorePause(1000) {
      if (System.currentTimeMillis() - start > 2000) {
        _show_game_saved_message = false
        mainScreen.deleteSelf()
      }
    }
  }

  private def loadSuccess(): Unit = {
    _show_game_loaded_message = true
    val start = System.currentTimeMillis()
    mainScreen.actionStaticPeriodIgnorePause(1000) {
      if (System.currentTimeMillis() - start > 2000) {
        _show_game_loaded_message = false
        mainScreen.deleteSelf()
      }
    }
  }

  private def loadFailed(): Unit = {
    _show_game_failed_to_load_message = true
    val start = System.currentTimeMillis()
    mainScreen.actionStaticPeriodIgnorePause(1000) {
      if (System.currentTimeMillis() - start > 2000) {
        _show_game_failed_to_load_message = false
        mainScreen.deleteSelf()
      }
    }
  }
}
