package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.AABB

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Planet(var m:Double, var coord:Vec, var vel:Vec) {
  val r:Float = 1/*+m*0.001*/

  def aabb:AABB = AABB(coord, r*2, r*2)
  def isCollided(op:Planet):Boolean = coord.dist(op.coord) < r + op.r

  var new_coord = coord
}

class MySpace(val bodies:Seq[Planet], val center:Vec, val width:Double, val height:Double) {
  def this(bodies:Seq[Planet], center:Vec) = {
    this(bodies, center, {
      val (init_min_x, init_max_x) = {
        bodies.headOption.map(b => {
          val AABB(c, w, _) = b.aabb
          (c.x - w/2, c.x+w/2)
        }).getOrElse((0.0, 0.0))
      }
      val (min_x, max_x) = bodies.foldLeft((init_min_x, init_max_x)) {
        case ((res_min_x, res_max_x), b) =>
          val AABB(c, w, _) = b.aabb
          val new_min_x = c.x - w/2
          val new_max_x = c.x + w/2
          (
            if(new_min_x < res_min_x) new_min_x else res_min_x,
            if(new_max_x > res_max_x) new_max_x else res_max_x
            )
      }
      math.max(math.abs(max_x - center.x)*2, math.abs(center.x - min_x)*2)
    }, {
      val (init_min_y, init_max_y) = {
        bodies.headOption.map(b => {
          val AABB(c, _, h) = b.aabb
          (c.y-h/2, c.y+h/2)
        }).getOrElse((0.0, 0.0))
      }
      val (min_y, max_y) = bodies.foldLeft(init_min_y, init_max_y) {
        case ((res_min_y, res_max_y), b) =>
          val AABB(c, _, h) = b.aabb
          val new_min_y = c.y - h/2
          val new_max_y = c.y + h/2
          (
            if(new_min_y < res_min_y) new_min_y else res_min_y,
            if(new_max_y > res_max_y) new_max_y else res_max_y
            )
      }
      math.max(math.abs(max_y - center.y)*2, math.abs(center.y - min_y)*2)
    })
  }

  lazy val aabb:AABB = AABB(center, width, height)

  lazy val quadSpaces:List[MySpace] = {
    val AABB(c, w, h) = aabb

    val c1 = c.toVec + Vec(-w/4, -h/4)
    val aabb1 = AABB(c1, w/2, h/2)
    val bodies1 = bodies.filter(b => b.aabb.aabbCollision(aabb1))

    val c2 = c.toVec + Vec(-w/4, h/4)
    val aabb2 = AABB(c2, w/2, h/2)
    val bodies2 = bodies.filter(b => b.aabb.aabbCollision(aabb2))

    val c3 = c.toVec + Vec(w/4, h/4)
    val aabb3 = AABB(c3, w/2, h/2)
    val bodies3 = bodies.filter(b => b.aabb.aabbCollision(aabb3))

    val c4 = c.toVec + Vec(w/4, -h/4)
    val aabb4 = AABB(c4, w/2, h/2)
    val bodies4 = bodies.filter(b => b.aabb.aabbCollision(aabb4))

    List(
      new MySpace(bodies1, c1, w/2, h/2),
      new MySpace(bodies2, c2, w/2, h/2),
      new MySpace(bodies3, c3, w/2, h/2),
      new MySpace(bodies4, c4, w/2, h/2)
    )
  }
}

object GenesisTest extends ScageScreenApp("Genesis Test", 800, 600) {
  private val G:Double = 1
  private val dt = 0.1

  private val to_remove = mutable.HashSet[Planet]()
  private val to_add = mutable.HashSet[Planet]()
  private val planets = ArrayBuffer[Planet]()

  private var _record_points = false
  private val points = ArrayBuffer[(Vec, Planet)]()

  private def randomCoord:Vec = {
    Vec((math.random*800).toFloat, (math.random*600).toFloat)
  }

  (1 to 3000).foreach(i => {
    planets += new Planet(1, randomCoord, Vec.zero)
  })

  /*planets += new Planet(5, windowCenter +Vec(-10,0), Vec.zero)
  planets += new Planet(5, windowCenter +Vec(10,0), Vec.zero)*/

  private var _center = center
  private var _selected_planet = 0

  private var id = 1
  private def getId: Int = {
    try {
      id
    } finally {
      id += 1
    }
  }

  private def splitMySpace(space:MySpace, max_level:Int, target:Int, level:Int = 0, spaces:List[MySpace] = Nil):List[MySpace] = {
    if(space.bodies.length <= target) {
      space :: spaces
    } else if(level > max_level) {
      space :: spaces
    } else {
      space.quadSpaces.flatMap {
        case s => splitMySpace(s, max_level, target, level+1, spaces)
      }
    }
  }

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center = center; _center += Vec(0, 5/globalScale); center = _center})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center = center; _center += Vec(-5/globalScale, 0); center = _center})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center = center; _center += Vec(0, -5/globalScale); center = _center})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center = center; _center += Vec(5/globalScale, 0); center = _center})
  keyIgnorePause(KEY_SPACE, onKeyDown = {
    _selected_planet += 1
    if(_selected_planet >= planets.length) _selected_planet = 0
    val p = planets(_selected_planet)
    center = p.coord
  })
  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL)) stopApp())
  keyIgnorePause(KEY_P, onKeyDown = switchPause())
  keyIgnorePause(KEY_1, onKeyDown = _record_points = !_record_points)
  keyIgnorePause(KEY_2, onKeyDown = points.clear())
  keyIgnorePause(KEY_3, onKeyDown = points --= {
    points.groupBy(_._2).flatMap(kv => kv._2.zipWithIndex.filter(x => x._2 % 2 == 0).map(_._1))
  })

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01) {
      if(globalScale.toInt > 100000) globalScale -= 100000
      else if(globalScale.toInt > 10000) globalScale -= 10000
      else if(globalScale.toInt > 1000) globalScale -= 1000
      else if(globalScale.toInt > 100) globalScale -= 100
      else if(globalScale.toInt > 10) globalScale -= 10
      else if(globalScale.toInt > 1) globalScale -= 1
      else if((globalScale*10).toInt > 1) globalScale -= 0.1f
      else globalScale -= 0.01f
      if(globalScale < 0.01) globalScale = 0.01f
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 1000000) {
      if(globalScale < 0.1) globalScale += 0.01f
      else if(globalScale < 1) globalScale += 0.1f
      else if(globalScale < 10) globalScale +=1
      else if(globalScale < 100) globalScale +=10
      else if(globalScale < 1000) globalScale +=100
      else if(globalScale < 10000) globalScale +=1000
      else if(globalScale < 100000) globalScale +=10000
      else globalScale += 100000
    }
    println(globalScale)
  })

  action {
    /*planets.zipWithIndex.foreach {
      case (p, idx) =>
        if(!to_remove.contains(p)) {
          val collided = (planets.take(idx) ++ planets.drop(idx + 1)).filter(op => !to_remove.contains(op) && p.coord.dist(op.coord) < p.r + op.r)
          if (collided.nonEmpty) {
            to_remove += p
            to_remove ++= collided
            to_add += new Planet(
              p.m + collided.map(_.m).sum,
              (p.coord + collided.map(_.coord).sum) / (collided.length + 1),
              (p.m * p.vel + collided.map(op => op.m * op.vel).sum) / (p.m + collided.map(_.m).sum))
          }
        }
    }*/

    (for {
      space <- {val ans = splitMySpace(new MySpace(planets, windowCenter), 5, 20); println(s"spaces = ${ans.length}; ${ans.map(_.bodies.length).mkString(" : ")}"); ans}
      if space.bodies.length > 1
      (p, idx) <- space.bodies.zipWithIndex.init
      op <- space.bodies.drop(idx + 1)
      if p.isCollided(op)
    } yield {
      List((p, op), (op, p))
    }).flatten.groupBy(_._1).foreach {
      case (p, collidedd) =>
        if(!to_remove.contains(p)) {
          val collided = collidedd.map(_._2).toSet
          to_remove += p
          to_remove ++= collided
          val newp = new Planet(
            p.m + collided.map(_.m).sum,
            (p.coord + collided.map(_.coord).sum) / (collided.size + 1),
            (p.m * p.vel + collided.map(op => op.m * op.vel).sum) / (p.m + collided.map(_.m).sum))
          to_add += newp
        }
    }

    planets --= to_remove
    planets ++= to_add
    to_remove.clear()
    to_add.clear()

    planets.zipWithIndex.foreach {
      case (p, idx) =>
        val force = (planets.take(idx) ++ planets.drop(idx+1)).map {
          case op => {
            G*p.m*op.m/p.coord.dist2(op.coord)*(op.coord - p.coord).n
          }
        }.sum
        val acc = force/p.m
        p.vel += acc*dt
        p.new_coord = p.coord + p.vel*dt
    }
    planets.foreach(p => {
      p.coord = p.new_coord
      if(_record_points) points += ((p.coord - center, p))
    })
  }

  center = _center

  render {
    planets.foreach {
      case p =>
        drawCircle(p.coord, p.r.toFloat, WHITE)
        if(globalScale >= 8) {
          print(p.m, p.coord, max_font_size / globalScale, WHITE, "center")
        }
    }

    points.foreach(p => drawFilledCircle(center + p._1, 1, WHITE))
  }

  interface {
    if(onPause) print("Пауза", windowCenter.toVec, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$ticks", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec*fps/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%%/${1*averageActionTimeMsec*ticks/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%%", windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec", windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec", windowWidth - 20, windowHeight - 100, align = "top-right", color = DARK_GRAY)
    print(s"planets: ${planets.size}", 20, 20, GRAY)
  }
}
