package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

class Planet(var m:Double, var coord:Vec, var vel:Vec) {
  val r = 1/*+m*0.001*/
  var new_vel:Vec = vel
  var new_coord:Vec = coord
  def swap(): Unit = {
    vel = new_vel
    coord = new_coord
  }
}

object GenesisTest extends ScageScreenApp("Genesis Test", 800, 600) {
  private val G:Double = 1
  private val dt = 0.1

  private val to_remove = ArrayBuffer[Planet]()
  private val to_add = ArrayBuffer[Planet]()
  private val planets = ArrayBuffer[Planet]()

  private var _record_points = false
  private val points = ArrayBuffer[Vec]()

  private def randomCoord:Vec = {
    Vec((math.random*800).toFloat, (math.random*600).toFloat)
  }

  (1 to 1000).foreach(i => {
    planets += new Planet(1, randomCoord, Vec.zero)
  })

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

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center += Vec(0, 5/globalScale); center = _center})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center += Vec(-5/globalScale, 0); center = _center})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center += Vec(0, -5/globalScale); center = _center})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center += Vec(5/globalScale, 0); center = _center})
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
  keyIgnorePause(KEY_3, onKeyDown = points --= points.zipWithIndex.filter(x => x._2 % 2 == 0).map(_._1))

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
    planets.zipWithIndex.foreach {
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
    }

    planets --= to_remove
    planets ++= to_add
    to_remove.clear()
    to_add.clear()

    planets.zipWithIndex.foreach {
      case (p, idx) =>
        val force = (planets.take(idx) ++ planets.drop(idx+1)).map {
          case op => G*p.m*op.m/p.coord.dist2(op.coord)*(op.coord - p.coord).n
        }.sum
        val acc = force/p.m
        p.new_vel = p.vel + acc*dt
        p.new_coord = p.coord + p.new_vel*dt
    }
    planets.foreach(p => {
      p.swap()
      if(_record_points) points += p.coord - center
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

    points.foreach(p => drawFilledCircle(center + p, 1, WHITE))
  }

  interface {
    print(s"planets: ${planets.size}", 20, 20, GRAY)
  }
}
