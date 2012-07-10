package su.msk.dunno.scage.tutorials.snake

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.{State, Vec}
import com.weiglewilczek.slf4s.Logger
import net.scage.support.tracer3.{TraceTrait, ScageTracer}

object Snake extends ScageScreenApp("Snake") {
  private val log = Logger(this.getClass.getName)

  val tracer = new ScageTracer[SnakePart](solid_edges = false) {
    render {
      for{
        x <- 0 until N_x
        y <- 0 until N_y
        point = point_matrix(x)(y)
        if point.size > 0
        snake_part = point.head
      } snake_part.render()
    }
  }
  
  private var count = 0
  interface {
    print(count, 20, windowHeight-20, GREEN)
  }

  private var dir = Vec(0, 1)
  private var head = tracer.addTrace(Vec(tracer.N_x/2, tracer.N_y/2), new SnakePart {
    is_head = true
    was_taken = true
  })

  key(KEY_UP,    onKeyDown = dir = Vec(0,1))
  key(KEY_DOWN,  onKeyDown = dir = Vec(0,-1))
  key(KEY_RIGHT, onKeyDown = dir = Vec(1,0))
  key(KEY_LEFT,  onKeyDown = dir = Vec(-1,0))

  val game_speed = property("game.speed", 100)
  action(game_speed) {
    tracer.tracesInPoint(tracer.outsidePoint(head.location + dir)).find(!_.wasTaken) match {
      case Some(part_in_point) => {
        part_in_point.changeState(head, new State("taken"))
        part_in_point.nextPart = head
        head = part_in_point
        count += 1
        deployNewPart()
      }
      case _ =>
    }
    nextMove(head, head.location + dir)
  }

  def nextMove(part:SnakePart, new_position:Vec) {
    if(tracer.tracesInPoint(tracer.outsidePoint(new_position)).isEmpty) {
      val previous_position = part.location.copy
      tracer.updateLocation(part, new_position)
      if(part.nextPart != null) nextMove(part.nextPart, previous_position)
    }
  }

  def deployNewPart() {
    action(game_speed) {
      val random_point = tracer.randomPoint()
      if(tracer.tracesInPoint(random_point).isEmpty) {
        tracer.addTrace(random_point, new SnakePart)
        deleteSelf()
      } else log.info("wrong place! Will choose another one on next turn")
    }
  }
  deployNewPart()
}

import Snake._

class SnakePart extends TraceTrait {
  private var next_part:SnakePart = null
  def nextPart_=(p:SnakePart) {next_part = p}
  def nextPart = next_part

  protected var is_head = false
  def isHead = is_head

  protected var was_taken = false
  def wasTaken = was_taken

  def state = new State()
  type ChangerType = SnakePart
  def changeState(changer:SnakePart, s:State) {
    s.neededKeys {
      case ("taken", true) =>
        was_taken = true
        is_head = true
        changer.changeState(this, new State("nohead"))
      case ("nohead", true) => is_head = false
    }
  }

  def render() {
    /*if(was_taken) {
      if(is_head) drawFilledRect(tracer.pointCenter(point), tracer.h_x, tracer.h_y, DARK_GREEN)
      else */drawFilledRectCentered(tracer.pointCenter(location), tracer.h_x, tracer.h_y, GREEN)
    /*}
    else drawFilledRect(tracer.pointCenter(point), tracer.h_x, tracer.h_y, RED)*/
  }
}
