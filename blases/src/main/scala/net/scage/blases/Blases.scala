package net.scage.blases

import net.scage.ScageLib._
import net.scage.support.{State, Vec}
import net.scage.{Scage, ScageScreenApp}
import net.scage.support.physics.{Physical, ScagePhysics}
import net.scage.support.tracer3.{TraceTrait, Trace, CoordTracer}
import net.scage.support.physics.objects.{StaticLine, DynaBall, StaticPolygon}
import collection.mutable.ArrayBuffer

object Blases extends ScageScreenApp("Blases") {
  val physics = ScagePhysics()
  val tracer = CoordTracer.create[Blase](solid_edges = false)

  val right_edge = new StaticLine(Vec(0,0), Vec(0, window_height))
  val up_edge    = new StaticLine(Vec(0, window_height), Vec(window_width, window_height))
  val left_edge  = new StaticLine(Vec(window_width, window_height), Vec(window_width, 0))
  val down_edge  = new StaticLine(Vec(window_width, 0), Vec(0, 0))

  physics.addPhysicals(right_edge, up_edge, left_edge, down_edge)

  private var current_level = 0
  private val _levels = ArrayBuffer[Level](new Level1)

  private var count = 0
  private var count_for_level = 0
  private var blases_created_in_level = 0
  private var blases_bursted = 0
  private var level_start_moment = 0L
  private var level_time = 0L

  def calculateFinalCount() {
    level_time = System.currentTimeMillis() - level_start_moment
    println(level_time)
  }
  
  action {
    physics.step()
    checkGameStatus()
    if(current_game_status != IN_PLAY) {
      pause()
      calculateFinalCount()
    }
  }
  
  val IN_PLAY = 0
  val WIN     = 1
  val LOSE    = 2
  private var current_game_status = IN_PLAY
  def checkGameStatus() {
    if(is_game_started && tracer.tracesList.isEmpty) current_game_status = LOSE
    if(_levels(current_level).isWin) current_game_status = WIN
  }
  
  interface {
    if(onPause) {
      current_game_status match {
        case WIN => print("You win! Play Again? (Y/N)", windowCenter, RED)
        case LOSE => print("You lose. Play Again? (Y/N)", windowCenter, RED)
        case _ =>
      }
    }
  }

  keyNoPause(KEY_Y, onKeyDown = if(onPause && current_game_status != IN_PLAY) {
    current_game_status match {
      case WIN =>
        current_level += 1
        if(current_level >= _levels.length) current_level = 0
      case _ =>
    }
    restart()
    pauseOff()
  })
  keyNoPause(KEY_N, onKeyDown = if(onPause && (current_game_status != IN_PLAY)) Scage.stopApp())
  
  key(KEY_SPACE, onKeyDown = if(selected_blase != no_selection) selected_blase.velocity = Vec.zero)
  
  render {
    if(!is_game_started) drawLine(start_coord, (mouseCoord - start_coord).n*40 + start_coord, RED)
    else if(selected_blase.id != no_selection.id) drawLine(selected_blase.location, (mouseCoord - selected_blase.location).n*40 + selected_blase.location, RED)
  }

  private val no_selection = new DynaBall(Vec.zero, radius = 20) with TraceTrait {
    def state = State()
    type ChangerType = Trace
    def changeState(changer:Trace, s:State) {}
  }
  private[blases] var selected_blase = no_selection

  leftMouse(onBtnDown = {mouse_coord =>
    if(!is_game_started) {
      val new_blase_position = (mouse_coord - start_coord).n*50 + start_coord
      val new_blase = new Blase(new_blase_position)
      new_blase.velocity = (mouse_coord - start_coord).n*90
      is_game_started = true
    } else if(selected_blase.id == no_selection.id) {
      val blases = tracer.tracesNearCoord(mouse_coord, -1 to 1, condition = {blase => blase.location.dist(mouse_coord) <= 20})
      if(!blases.isEmpty) {
        selected_blase = blases.head
      }
    } else {
      val new_blase_position = (mouse_coord - selected_blase.location).n*50 + selected_blase.location
      val new_blase = new Blase(new_blase_position)
      new_blase.velocity = (mouse_coord - selected_blase.location).n*90
      selected_blase = no_selection
    }
  })

  rightMouse(onBtnDown = {mouse_coord => selected_blase = no_selection})

  private var is_game_started = false
  private val start_coord = Vec(271, 564)
  init {
    _levels(current_level).load()
    is_game_started = false
    current_game_status = IN_PLAY
    count_for_level = 0
    blases_created_in_level = 0
    blases_bursted = 0
    level_start_moment = System.currentTimeMillis()
  }
  
  clear {
    tracer.tracesList.foreach(_.burst())
  }
}

import Blases._

class Blase(init_coord:Vec) extends DynaBall(init_coord, radius = 20) with Trace {
  physics.addPhysical(this)
  tracer.addTrace(coord, this)
  body.setUserData(this)

  def state = State()
  def changeState(changer:Trace, s:State) {}

  private val action_id = action {
    tracer.updateLocation(this, coord)
    coord = this.location
    if(!touchingPoints.isEmpty) velocity = Vec.zero
  }

  def burst() {
    delOperations(action_id, render_id)
    tracer.removeTraces(this)
    physics.removePhysicals(this)
  }
  
  private val render_id = render {
    val color = if(id == selected_blase.id) RED else WHITE
    drawCircle(coord, radius, color)
  }
}

class BurstPolygon(vertices:Vec*) extends StaticPolygon(vertices:_*) {
  physics.addPhysical(this)

  private val action_id = action {
    touchingBodies.foreach{body => {
      val user_data = body.getUserData
      if(user_data != null && user_data.isInstanceOf[Blase]) {
        user_data.asInstanceOf[Blase].burst()
      }
    }}
  }

  private val render_id = render {
    drawPolygon(points, RED)
  }

  def remove() {
    physics.removePhysicals(this)
    delOperations(action_id, render_id)
  }
}

class SpeedPolygon(vertices:Array[Vec], direction:Vec) {
  private val dir = direction.n*90
  private val (min_x, max_x, min_y, max_y) = vertices.map(vertice => tracer.outsidePoint(tracer.point(vertice))).foldLeft((0, 0, 0, 0)) {
    case ((current_min_x, current_max_x, current_min_y, current_max_y), vertice) =>
      val new_min_x = math.min(current_min_x, vertice.ix)
      val new_max_x = math.max(current_max_x, vertice.ix)
      val new_min_y = math.min(current_min_y, vertice.iy)
      val new_max_y = math.max(current_max_y, vertice.iy)
      (new_min_x, new_max_x, new_min_y, new_max_y)
  }

  private val speeded_blases_ids = ArrayBuffer[Int]()
  private val action_id = action {
    tracer.tracesInPointRange(min_x to max_x, min_y to max_y).filter(blase => containsCoord(blase.location)).foreach(blase => {
      if(!speeded_blases_ids.contains(blase.id)) {
        blase.velocity = blase.velocity + dir
        speeded_blases_ids += blase.id
      }
    })
  }

  private val vertices_array = vertices.toArray
  private val render_id = render {
    drawPolygon(vertices_array, BLUE)
  }

  def containsCoord(coord:Vec):Boolean = {
    def _areLinesIntersect(a:Vec, b:Vec, c:Vec, d:Vec):Boolean = {
      val common = (b.x - a.x)*(d.y - c.y) - (b.y - a.y)*(d.x - c.x)
      if (common == 0) false
      else {
        val rH = (a.y - c.y)*(d.x - c.x) - (a.x - c.x)*(d.y - c.y)
        val sH = (a.y - c.y)*(b.x - a.x) - (a.x - c.x)*(b.y - a.y)

        val r = rH / common
        val s = sH / common

        if(r >= 0 && r <= 1 && s >= 0 && s <= 1) true
        else false
      }
    }
    if(vertices_array.length < 2) false
    else _areLinesIntersect(coord, Vec(Integer.MAX_VALUE, coord.y), vertices_array(0), vertices_array(1))
  }

  def remove() {
    delOperations(action_id, render_id)
  }
}

trait Level {
  def load()
  def isWin:Boolean
}

class Level1 extends Level {
  def load() {
    val first  = new StaticPolygon(Vec(84,  212), Vec(458, 564), Vec(627, 393), Vec(591, 359), Vec(454, 495), Vec(113, 175))
    val second = new StaticPolygon(Vec(76,  85),  Vec(810, 83),  Vec(812,46),   Vec(77,  45))
    val third  = new StaticPolygon(Vec(782, 658), Vec(829, 658), Vec(830,151),  Vec(787, 152))
    val fourth = new StaticPolygon(Vec(562, 281), Vec(615, 256), Vec(644,186),  Vec(568, 150), Vec(536, 223))

    physics.addPhysicals(first, second, third, fourth)

    val burst_polygon = new BurstPolygon(Vec(772, 153), Vec(787, 140), Vec(745, 80), Vec(731, 95))

    val render_id = render {
      currentColor = WHITE
      drawPolygon(first.points)
      drawPolygon(second.points)
      drawPolygon(third.points)
      drawPolygon(fourth.points)

      drawCircle(Vec(271, 564), 20, RED)
      print("Start", 250, 520, RED)

      drawCircle(Vec(350, 300), 30, GREEN)
      print("Finish", 325, 250, GREEN)
    }

    clear {
      physics.removePhysicals(first, second, third, fourth)
      burst_polygon.remove()
      delOperation(render_id)
      deleteSelf()
    }
  }

  def isWin:Boolean = {
    val winner_blases = tracer.tracesNearCoord(Vec(350, 300), -1 to 1, condition = {blase => blase.location.dist(Vec(350, 300)) < 20})
    !winner_blases.isEmpty
  }
}
