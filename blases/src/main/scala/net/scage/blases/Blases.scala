package net.scage.blases

import levelparts.FlyingWord
import levels._
import net.scage.ScageLib._
import net.scage.support.{State, Vec}
import net.scage.support.physics.ScagePhysics
import net.scage.support.tracer3.{TraceTrait, Trace, CoordTracer}
import net.scage.support.physics.objects.{StaticLine, DynaBall}
import net.scage.blases.Relatives._
import ui.PauseMenu
import net.scage.Screen
import net.scage.handlers.controller2.MultiController
import collection.mutable.{Stack, ArrayBuffer}

object LevelSelector {
  private var current_level_num = 0
  private val levels = ArrayBuffer(Level1,
                                   Level2,
                                   Level3,
                                   Level4,
                                   Level5,
                                   Level6,
                                   BonusLevel1,
                                   TestLevel,
                                   Level7,
                                   Level8)
  
  def currentLevel = levels(current_level_num)

  def currentLevelNum = current_level_num+1
  def currentLevelNum_=(new_level_num:Int) {
    val level_num = new_level_num-1
    if(level_num >= levels.length || level_num < 0) currentLevelNum = 0
    else if(!levels(level_num).is_entered) {
      levels(level_num) match {
        case bonus_level:BonusLevel =>
          if(bonus_level.bonusCondition) current_level_num = level_num
          else currentLevelNum = new_level_num + 1
        case _ => current_level_num = level_num
      }
    } else current_level_num = level_num
  }

  def currentLevelScore = levels(current_level_num).score_for_level
  def currentLevelScore_=(new_score_for_level:Int) {
    levels(current_level_num).score_for_level = new_score_for_level
  }

  def currentLevelBlases = levels(current_level_num).blases_shot_on_level
  def currentLevelBlases_=(new_blases_shot_on_level:Int) {
    levels(current_level_num).blases_shot_on_level = new_blases_shot_on_level
  }

  def levelStats(level_num:Int) = {
    if(level_num >= levels.length || level_num < 0) (0, 0)
    else (levels(current_level_num).score_for_level, levels(current_level_num).blases_shot_on_level)
  }

  def isLastLevel = current_level_num == levels.length-1
}

object BlaseSelector {
  private val no_selection = new DynaBall(Vec.zero, radius = rInt(20)) with Trace {
    def state = State()
    def changeState(changer:Trace, s:State) {}
    def bursted = false
  }

  private var selected_blase = no_selection
  private val previous_selections = Stack[DynaBall with Trace {def bursted:Boolean}]()

  def selectedBlase_=(new_selected_blase:Blase) {
    previous_selections.push(selected_blase)
    selected_blase = new_selected_blase
  }

  def selectedBlase = selected_blase

  def previousSelection() {
    selected_blase = previous_selections.pop()
    if(selected_blase.bursted) previousSelection()
  }

  def noBlaseSelected = selected_blase == no_selection

  def clearSelectionHistory() {
    previous_selections.clear()
    selected_blase = no_selection
  }
}

import LevelSelector._
import BlaseSelector._

object Blases extends Screen("Blases Game") with MultiController {
  val physics = ScagePhysics()
  private var _tracer = CoordTracer.create[Blase](field_from_x = 0,
                                                  field_to_x = windowWidth,
                                                  field_from_y = 0,
                                                  field_to_y = windowHeight,
                                                  init_h_x = rInt(50), init_h_y = rInt(50),
                                                  solid_edges = false)
  def tracer = _tracer
  preinit {
    _tracer = CoordTracer.create[Blase](field_from_x = 0,
                                        field_to_x = windowWidth,
                                        field_from_y = 0,
                                        field_to_y = windowHeight,
                                        init_h_x = rInt(50), init_h_y = rInt(50),
                                        solid_edges = false)
  }

  private[blases] var score = 0
  private[blases] var score_for_level = 10000
  private[blases] var score_updated = false

  private[blases] var blases_shot = 0
  private[blases] var blases_shot_on_level = 0

  action(1000) {
    if(is_game_started) score_for_level -= 50
  }
  
  action {
    physics.step()

    if(is_game_started) {
      if(tracer.tracesList.isEmpty) {
        score += score_for_level
        score_updated = true
        PauseMenu.showLoseLevelMenu()
      } else {
        val winner_blases = currentLevel.finishCoords.map(finish_coord => tracer.tracesNearCoord(finish_coord, -1 to 1, condition = {
          blase => blase.location.dist(finish_coord) < rInt(30)
        })).flatten
        if(!winner_blases.isEmpty) {
          new FlyingWord(score_for_level, YELLOW, winner_blases.head.location, winner_blases.head.velocity)
          if(score_for_level > currentLevelScore) currentLevelScore = score_for_level
          if(blases_shot_on_level < currentLevelBlases) currentLevelBlases = blases_shot_on_level
          score += score_for_level
          score_updated = true
          if(isLastLevel) PauseMenu.showBeatGameMenu()
          else PauseMenu.showWinLevelMenu()
        }
      }
    }
  }

  keyNoPause(KEY_ESCAPE, onKeyDown = {
    PauseMenu.status match {
      case -1 => PauseMenu.showEscMenu()
      case PauseMenu.PRESS_ESC => PauseMenu.hide()
      case _ =>
    }
  })
  key(KEY_SPACE, onKeyDown = if(!noBlaseSelected) selectedBlase.velocity = Vec.zero)

  private var is_shift_pressed = false
  key(KEY_LSHIFT, onKeyDown = is_shift_pressed = true, onKeyUp = is_shift_pressed = false)
  key(KEY_RSHIFT, onKeyDown = is_shift_pressed = true, onKeyUp = is_shift_pressed = false)
  
  render {
    if(!is_game_started) drawLine(currentLevel.startCoord, (mouseCoord - currentLevel.startCoord).n*rInt(40) + currentLevel.startCoord, rColor(RED))
    else if(!noBlaseSelected) drawLine(selectedBlase.location, (mouseCoord - selectedBlase.location).n*rInt(40) + selectedBlase.location, rColor(RED))
  }

  interface {
    print(xml("blases.score", score),  20, windowHeight-20, rColor(WHITE))
 	  //print(score_for_level,  20, windowHeight-40, rColor(WHITE))
    print(xml("blases.bubbles", blases_shot),  20, windowHeight-40, rColor(WHITE))

    //print(fps, 20, windowHeight-40, WHITE)

    //drawTraceGrid(tracer, DARK_GRAY)
    //print(mouseCoord, 20, 20, GREEN)
  }

  leftMouse(onBtnDown = {mouse_coord =>
    if(!is_game_started) {
      val new_blase_position = (mouse_coord - currentLevel.startCoord).n*rInt(45) + currentLevel.startCoord
      val new_blase = new Blase(new_blase_position, mouse_coord - currentLevel.startCoord)
      selectedBlase = new_blase
      is_game_started = true
      blases_shot += 1
      blases_shot_on_level += 1
    } else {
      val blases = tracer.tracesNearCoord(mouse_coord, -1 to 1, condition = {blase => blase.location.dist(mouse_coord) <= 20})
      if(!blases.isEmpty) {
        selectedBlase = blases.head
      } else if(!noBlaseSelected) {
        val new_blase_position = (mouse_coord - selectedBlase.location).n*rInt(45) + selectedBlase.location
        val new_blase = new Blase(new_blase_position, mouse_coord - selectedBlase.location)
        if(!is_shift_pressed) selectedBlase = new_blase
        blases_shot += 1
        blases_shot_on_level += 1
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

    val action_id = action {
      (right_edge.touchingBodies ++ up_edge.touchingBodies ++ left_edge.touchingBodies ++ down_edge.touchingBodies).foreach {
        body => {
          val user_data = body.getUserData
          if (user_data != null && user_data.isInstanceOf[Blase]) {
            user_data.asInstanceOf[Blase].velocity = Vec.zero
          }
        }
      }
    }

    dispose {
      physics.removePhysicals(right_edge, up_edge, left_edge, down_edge)
      delActions(action_id)
      deleteSelf()
    }

    score = 0
    blases_shot = 0

    PauseMenu.hide()
  }

  private var is_game_started = false
  init {
    currentLevel.load()
    currentLevel.is_entered = true
    is_game_started = false
    clearSelectionHistory()
    score_for_level = 10000
    blases_shot_on_level = 0
    score_updated = false
  }
  
  clear {
    tracer.tracesList.foreach(_.burst())
  }
}














