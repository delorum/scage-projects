package net.scage.blases

import levelparts.FlyingWord
import levels._
import net.scage.ScageLib._
import net.scage.support.{State, Vec}
import net.scage.support.physics.ScagePhysics
import net.scage.support.tracer3.{TraceTrait, Trace, CoordTracer}
import net.scage.support.physics.objects.{StaticLine, DynaBall}
import collection.mutable.ArrayBuffer
import net.scage.blases.Relatives._
import ui.PauseMenu
import net.scage.Screen
import net.scage.handlers.controller2.MultiController

object Blases extends Screen("Blases Game") with MultiController {
  val physics = ScagePhysics()
  val tracer = CoordTracer.create[Blase](solid_edges = false)

  private[blases] var current_level = 0
  private[blases] val levels = ArrayBuffer(Level1, Level2, Level3, Level4, Level5)

  private[blases] var score = 0
  private[blases] var score_for_level = 10000
  private[blases] var blases_shot = 0
  action(1000) {
    if(is_game_started) score_for_level -= 50
  }
  
  action {
    physics.step()

    if(is_game_started && tracer.tracesList.isEmpty) {
      score += score_for_level
      PauseMenu.showLoseLevelMenu()
    }
    else if(levels(current_level).isWin) {
      score += score_for_level
      if(current_level == levels.length-1) PauseMenu.showBeatGameMenu()
      else PauseMenu.showWinLevelMenu()
    }
  }

  keyNoPause(KEY_ESCAPE, onKeyDown = {
    PauseMenu.status match {
      case -1 => PauseMenu.showEscMenu()
      case PauseMenu.PRESS_ESC => PauseMenu.hide()
      case _ =>
    }
  })
  key(KEY_SPACE, onKeyDown = if(selected_blase != no_selection) selected_blase.velocity = Vec.zero)

  private var is_shift_pressed = false
  key(KEY_LSHIFT, onKeyDown = is_shift_pressed = true, onKeyUp = is_shift_pressed = false)
  key(KEY_RSHIFT, onKeyDown = is_shift_pressed = true, onKeyUp = is_shift_pressed = false)
  
  render {
    if(!is_game_started) drawLine(levels(current_level).startCoord, (mouseCoord - levels(current_level).startCoord).n*rInt(40) + levels(current_level).startCoord, RED)
    else if(selected_blase.id != no_selection.id) drawLine(selected_blase.location, (mouseCoord - selected_blase.location).n*rInt(40) + selected_blase.location, RED)
  }

  interface {
    print("Score: "+score,  20, windowHeight-20, WHITE)
 	  print(score_for_level,  20, windowHeight-40, WHITE)
  }

  private[blases] val no_selection = new DynaBall(Vec.zero, radius = 20) with TraceTrait {
    def state = State()
    type ChangerType = Trace
    def changeState(changer:Trace, s:State) {}
  }
  private[blases] var selected_blase = no_selection


  leftMouse(onBtnDown = {mouse_coord =>
    if(!is_game_started) {
      val new_blase_position = (mouse_coord - levels(current_level).startCoord).n*50 + levels(current_level).startCoord
      val new_blase = new Blase(new_blase_position, mouse_coord - levels(current_level).startCoord)
      selected_blase = new_blase
      is_game_started = true
      blases_shot += 1
    } else {
      val blases = tracer.tracesNearCoord(mouse_coord, -1 to 1, condition = {blase => blase.location.dist(mouse_coord) <= 20})
      if(!blases.isEmpty) {
        selected_blase = blases.head
      } else {
        val new_blase_position = (mouse_coord - selected_blase.location).n*50 + selected_blase.location
        val new_blase = new Blase(new_blase_position, mouse_coord - selected_blase.location)
        if(!is_shift_pressed) selected_blase = new_blase
        blases_shot += 1
      }
    }
  })

  rightMouse(onBtnDown = {mouse_coord =>
    if(tracer.tracesList.length > 1) {
      val blases = tracer.tracesNearCoord(mouse_coord, -1 to 1, condition = {blase => blase.location.dist(mouse_coord) <= 20})
      if(!blases.isEmpty) {
        blases.head.burst()
      }
    }
  })

  preinit {
    backgroundColor = BLACK
    val right_edge = new StaticLine(Vec(0,0), Vec(0, windowHeight))
    val up_edge    = new StaticLine(Vec(0, windowHeight), Vec(windowWidth, windowHeight))
    val left_edge  = new StaticLine(Vec(windowWidth, windowHeight), Vec(windowWidth, 0))
    val down_edge  = new StaticLine(Vec(windowWidth, 0), Vec(0, 0))
    physics.addPhysicals(right_edge, up_edge, left_edge, down_edge)

    dispose {
      physics.removePhysicals(right_edge, up_edge, left_edge, down_edge)
      deleteSelf()
    }

    score = 0
    current_level = 0

    PauseMenu.hide()
  }

  private var is_game_started = false
  init {
    levels(current_level).load()
    is_game_started = false
    selected_blase = no_selection
    score_for_level = 10000
  }
  
  clear {
    tracer.tracesList.foreach(_.burst())
  }
}














