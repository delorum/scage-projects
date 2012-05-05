package net.scageprojects.td

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import org.newdawn.slick.util.pathfinding.{Mover, PathFindingContext, TileBasedMap, AStarPathFinder}
import net.scage.support.tracer3.{CoordTracer, ScageTracer, Trace}
import collection.mutable.{ArrayBuffer, Stack}
import net.scage.support.{ScageColor, State, Vec}

object TrafalgarDefence extends ScageScreenApp("Trafalgar Defence", 800, 600) {
  val tracer = CoordTracer(
    field_from_x = 10,
    field_to_x = 610,
    field_from_y = 10,
    field_to_y = 590,
    init_h_x = 20,
    init_h_y = 20
  )
  val path_finder = new AStarPathFinder(new TileBasedMap {
    def getWidthInTiles = tracer.N_x
    def getHeightInTiles = tracer.N_y
    def pathFinderVisited(x:Int, y:Int) {}
    def blocked(context:PathFindingContext, tx:Int, ty:Int) = {
      tracer.tracesInPoint(tx, ty).exists(trace => trace.state.contains("impassable"))
    }
    def getCost(context:PathFindingContext, tx:Int, ty:Int) = 1f
  }, 100500, true)

  def findPath(p1:Vec, p2:Vec) = {
    val slick_path = path_finder.findPath(null, p1.ix, p1.iy, p2.ix, p2.iy)
    if(slick_path != null) Stack((for(i <- 0 until slick_path.getLength) yield tracer.pointCenter(Vec(slick_path.getX(i), slick_path.getY(i)))):_*)
    else Stack[Vec]()
  }

  render {
    drawTraceGrid(tracer, DARK_GRAY)
    drawTraceLocations(tracer, GREEN, 4)
    print(fps, 620, 580, WHITE)
  }

  private var first_point = Vec(-1, -1)
  private var test_path = ArrayBuffer[Vec]()

  leftMouse(onBtnDown = {m =>
    /*if(first_point == Vec(-1, -1)) first_point = tracer.point(m)
    else {
      val end_point = tracer.point(m)
      val slick_path = path_finder.findPath(new Mover {}, first_point.ix, first_point.iy, end_point.ix, end_point.iy)
      if(slick_path != null) {
        test_path = ArrayBuffer((for(i <- 0 until slick_path.getLength) yield tracer.pointCenter(Vec(slick_path.getX(i), slick_path.getY(i)))):_*)
        first_point = Vec(-1, -1)
      }
    }*/
    val p = tracer.point(m)
    new Tower(p)
  })

  render {
    test_path.foreach(p => drawFilledCircle(p, 3, YELLOW))
    if(first_point != Vec(-1, -1)) drawFilledCircle(tracer.pointCenter(first_point), 3, RED)
  }

  def wave(amount:Int) {
    for(i <- 0 until amount) {
      val start = Vec((math.random*tracer.N_x).toInt, tracer.N_y - 1)
      val end = Vec((math.random*tracer.N_x).toInt, 0)
      new Enemy(start, end)
    }
  }

  action (20000) {
    wave(50)
  }
}

import TrafalgarDefence._

class Tower(upper_left_point:Vec) {
  private def towerPart = new Trace {
    def state = State("impassable")
    def changeState(changer:Trace, s:State) {}
  }

  val tower_traces = tracer.addTraces((tracer.pointCenter(upper_left_point),              towerPart),
                                      (tracer.pointCenter(upper_left_point + Vec(1, 0)),  towerPart),
                                      (tracer.pointCenter(upper_left_point + Vec(0, -1)), towerPart),
                                      (tracer.pointCenter(upper_left_point + Vec(1, -1)), towerPart))
  callEvent("New Tower Placed")
  
  val tower_center = tracer.pointCenter(upper_left_point) + Vec(tracer.h_x/2, -tracer.h_y/2)

  private var last_shoot_time = 0L
  private var shoot_timeout = 200  // msec
  private def shoot(target:Trace) {
    if(msecsFrom(last_shoot_time) > shoot_timeout) {
      new Bullet(tower_center, target)
      last_shoot_time = msecs
    }
  }

  action {
    val enemies = tracer.tracesInPointRange(
      upper_left_point.ix - 2 to upper_left_point.ix + 3,
      upper_left_point.iy - 3 to upper_left_point.iy + 2,
      condition = {trace => trace.state.contains("enemy") && trace.state.value[Int]("hp") > 0})
    if(!enemies.isEmpty) {
      val nearest_enemy = enemies.foldLeft(enemies.head) {
        case (current_nearest, next_enemy) => if(next_enemy.location.dist(tower_center) < current_nearest.location.dist(tower_center)) next_enemy else current_nearest
      }
      shoot(nearest_enemy)
    }
  }
  
  render {
    drawRectCentered(tower_center, tracer.h_x*2, tracer.h_y*2, WHITE)
  }
}

class Enemy(init_point:Vec, end_point:Vec) extends Trace with Mover {
  private var hp = 100

  tracer.addTrace(tracer.pointCenter(init_point), this)

  private var path = findPath(init_point, end_point)
  private var next_coord = tracer.pointCenter(init_point)
  onEvent("New Tower Placed") {
    case _ =>
      path = findPath(tracer.point(location), end_point)
      if(!path.isEmpty) next_coord = path.pop()
  }

  action(10) {
    if(hp <= 0) deleteSelf()
    else {
      if((location dist next_coord) < 3) {
        if(!path.isEmpty) next_coord = path.pop()
        else {
          // do some damage to the player
          deleteSelf()
        }
      } else tracer.updateLocation(this, location + (next_coord - location).n)
    }
  }

  render {
    drawCircle(location, 10, if(hp > 0) WHITE else BLUE)
    drawFilledCircle(next_coord, 3, RED)
  }

  def state = State("enemy" -> true, "hp" -> hp)
  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("damage", damage:Int) =>
        hp -= damage
    }
  }
}

class Bullet(start_coord:Vec, target:Trace) {
  val speed = 3

  private var lifetime = 50
  private var coord = start_coord


  action(10) {
    coord += (target.location - coord).n*speed
    if(coord.dist(target.location) < 3) {
      val damage = (math.random*20).toInt
      target.changeState(null, State("damage" -> damage))
      delOperations(render_id, currentOperation)
      new FlyingWord(damage, RED, coord, (target.location - coord))
    } else {
      lifetime -= 1
      if(lifetime <= 0) {
        delOperations(render_id, currentOperation)
      }
    }
  }

  private val render_id = render {
    openglMove(coord)
    openglRotate(-(target.location - coord).deg(Vec(0,1)))
    drawRectCentered(Vec.zero, 5, 7, WHITE)
  }
}

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) {
  private var lifetime = 100;
  private val dir = direction.n
  private var coord = init_coord

  action(10) {
    if(lifetime > 0) {
      coord += dir
      lifetime -= 1
    } else deleteSelf()
  }

  render {
    if(lifetime > 0) {
      print(message, coord, color)
    } else deleteSelf()
  }
}
