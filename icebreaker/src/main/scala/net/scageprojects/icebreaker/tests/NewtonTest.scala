package net.scageprojects.icebreaker.tests

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.Vec
import collection.mutable.ArrayBuffer

object NewtonTest extends ScageScreenApp("Newton", 640, 480) {
  val dt = 1
  val G = 1

  val m1 = 1
  var vel1 = Vec(-1, 0)
  var coord1 = windowCenter + Vec(0, 150)

  val m2 = 100
  var vel2 = Vec.zero
  var coord2 = windowCenter

  def newtonForce(mass:Float, coord:Vec) = {
    val d = coord2 - coord
    G*mass*m2/(math.pow(d.norma, 3))*d
  }

  val coords = ArrayBuffer[(Vec, Vec)]()  // (vel, coord)

  action {
    val dvel1 = newtonForce(m1, coord1)*(1f/m1)
    vel1 += dvel1
    coord1 += vel1*dt

    for(i <- 0 until 10) {
      val (v, c) = if(coords.isEmpty) (vel1, coord1) else coords.last
      val dv = newtonForce(m1, c)*(1f/m1)
      val new_v = v + dv
      val new_c = c + new_v*dt
      coords += ((new_v, new_c))
    }
  }

  var drag_coord = Vec.zero
  leftMouseNoPause(
    onBtnDown = {m =>
      drag_coord = m
      pause()
    },
    onBtnUp = {m =>
      vel1 += (drag_coord - coord1)*0.01f
      drag_coord = Vec.zero
      coords.clear()
      pauseOff()
    }
  )

  leftMouseDragNoPause(onDrag = {m =>
    drag_coord = m
  })

  render {
    drawCircle(coord1, 10, WHITE)
    drawCircle(coord2, 20, WHITE)
    drawPoints(coords.map(_._2).toArray, GREEN)
    if( drag_coord != Vec.zero) drawLine(coord1, drag_coord, WHITE)
  }
}
