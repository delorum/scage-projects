package net.scage.blases

import levels.{Level3, Level2, Level1}
import net.scage.ScageLib._
import net.scage.support.{State, Vec}
import net.scage.support.physics.ScagePhysics
import net.scage.support.tracer3.{TraceTrait, Trace, CoordTracer}
import net.scage.support.physics.objects.{StaticLine, DynaBall, StaticPolygon}
import collection.mutable.{HashMap, ArrayBuffer}
import net.scage.ScageScreen
import net.scage.blases.Relatives._

object Blases extends ScageScreen("Blases Game") {
  val physics = ScagePhysics()
  val tracer = CoordTracer.create[Blase](solid_edges = false)

  private var current_level = 0
  private val levels = ArrayBuffer(Level1, Level2, Level3)

  private var score = 0
  private[blases] var score_for_level = 10000
  action(1000) {
    if(is_game_started && current_game_status == IN_PLAY) score_for_level -= 50
  }
  
  action {
    physics.step()
    checkGameStatus()
    if(current_game_status != IN_PLAY) {
      score += score_for_level
      pause()
    }
  }
  
  val IN_PLAY = 0
  val WIN     = 1
  val LOSE    = 2
  private var current_game_status = IN_PLAY
  def checkGameStatus() {
    if(is_game_started && tracer.tracesList.isEmpty) current_game_status = LOSE
    if(levels(current_level).isWin) current_game_status = WIN
  }
  
  interface {
    print("Score: "+score,  20, windowHeight-20, WHITE)
    print(score_for_level,  20, windowHeight-40, WHITE)
    if(onPause) {
      current_game_status match {
        case WIN => 
          if(current_level < levels.length-1) {
            print("You win! Score for the level:\n"+score_for_level+"\n\nOverall score:\n"+score+"\n\nWanna go to the next level? (Y/N)", windowCenter + Vec(-60, 60), RED)
          } else print("You beat the game!!! Final score:\n"+score+"\n\nWanna play again from the beginning? (Y/N)", windowCenter + Vec(-60, 60), RED)
        case LOSE => print("You lose. Final score:\n"+score+"\n\nWanna play the last level again? (Y/N)", windowCenter + Vec(-60, 60), RED)
        case _ =>
      }
    }
  }

  keyNoPause(KEY_Y, onKeyDown = if(onPause && current_game_status != IN_PLAY) {
    current_game_status match {
      case WIN =>
        if(current_level < levels.length-1) {
          current_level += 1
        } else {
          score = 0
          current_level = 0
        }
      case LOSE =>
        score -= score_for_level
      case _ =>
    }
    restart()
    pauseOff()
  })
  keyNoPause(KEY_N, onKeyDown = if(onPause && (current_game_status != IN_PLAY)) stop())
  
  key(KEY_SPACE, onKeyDown = if(selected_blase != no_selection) selected_blase.velocity = Vec.zero)
  
  render {
    if(!is_game_started) drawLine(levels(current_level).startCoord, (mouseCoord - levels(current_level).startCoord).n*rInt(40) + levels(current_level).startCoord, RED)
    else if(selected_blase.id != no_selection.id) drawLine(selected_blase.location, (mouseCoord - selected_blase.location).n*rInt(40) + selected_blase.location, RED)
  }

  private val no_selection = new DynaBall(Vec.zero, radius = 20) with TraceTrait {
    def state = State()
    type ChangerType = Trace
    def changeState(changer:Trace, s:State) {}
  }
  private[blases] var selected_blase = no_selection

  leftMouse(onBtnDown = {mouse_coord =>
    if(!is_game_started) {
      val new_blase_position = (mouse_coord - levels(current_level).startCoord).n*50 + levels(current_level).startCoord
      val new_blase = new Blase(new_blase_position)
      new_blase.velocity = (mouse_coord - levels(current_level).startCoord).n*rInt(90)
      is_game_started = true
    } else if(selected_blase.id == no_selection.id) {
      val blases = tracer.tracesNearCoord(mouse_coord, -1 to 1, condition = {blase => blase.location.dist(mouse_coord) <= 20})
      if(!blases.isEmpty) {
        selected_blase = blases.head
      }
    } else {
      val new_blase_position = (mouse_coord - selected_blase.location).n*50 + selected_blase.location
      val new_blase = new Blase(new_blase_position)
      new_blase.velocity = (mouse_coord - selected_blase.location).n*rInt(90)
      selected_blase = no_selection
    }
  })

  rightMouse(onBtnDown = {mouse_coord =>
    /*selected_blase = no_selection*/
    if(tracer.tracesList.length > 1) {
      val blases = tracer.tracesNearCoord(mouse_coord, -1 to 1, condition = {blase => blase.location.dist(mouse_coord) <= 20})
      if(!blases.isEmpty) {
        blases.head.burst()
        if(blases.head.id == selected_blase.id ) selected_blase = no_selection
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
    }

    score = 0
    current_level = 0
    pauseOff()
  }

  private var is_game_started = false
  init {
    levels(current_level).load()
    is_game_started = false
    current_game_status = IN_PLAY
    score_for_level = 10000
  }
  
  clear {
    tracer.tracesList.foreach(_.burst())
  }
}














