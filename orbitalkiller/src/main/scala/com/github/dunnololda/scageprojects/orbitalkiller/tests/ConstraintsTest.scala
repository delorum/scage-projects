package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.tests.BodyStatesHolder._

import scala.collection.mutable.ArrayBuffer

import com.github.dunnololda.scageprojects.orbitalkiller.tests.BodyStatesHolder._

object ConstraintsTest extends ScageScreenAppD("Constraints Test", 640, 480) {
  val base_dt = 1.0/63.0
  var dt:Int = 1

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(-1 + math.random*2, -1 + math.random*2).n*50f
  def energy(bodies:Seq[MutableBodyState]) =
    bodies.map(b1 => {
      b1.mass*b1.vel.norma2/2f +
      b1.I*b1.ang_vel.toRad*b1.ang_vel.toRad/2f/* +
      b1.currentState.mass*9.81*b1.currentState.coord.y*/
    }).sum

  val w1 = new MyWall2(nextId, Vec(w-100, h-100),  Vec(w-100, h+100), this)
  system_evolution.addBody(w1.currentState.toMutableBodyState)
  val w2 = new MyWall2(nextId, Vec(w-100, h+100),  Vec(w+100, h+100), this)
  val w2mbs = w2.currentState.toMutableBodyState
  system_evolution.addBody(w2mbs)
  val w3 = new MyWall2(nextId, Vec(w+100, h+100),  Vec(w+100, h-100), this)
  system_evolution.addBody(w3.currentState.toMutableBodyState)
  val w4 = new MyWall2(nextId, Vec(w+100, h-100),  Vec(w-100, h-100), this)
  system_evolution.addBody(w4.currentState.toMutableBodyState)

  def test1() {
    val c1 = new MyCircle2(nextId, DVec(w,h), DVec(0, -30), 5, 1f, this)
    val c1mbs = c1.currentState.toMutableBodyState
    system_evolution.addBody(c1mbs)
    val c2 = new MyCircle2(nextId, DVec(w+50,h), DVec(0, -20), 5, 1f, this)
    val c2mbs = c2.currentState.toMutableBodyState
    system_evolution.addBody(c2mbs)
    system_evolution.addJoint(c1mbs, DVec(25, 0), c2mbs, DVec(-25, 0))
    val bodies = List(c1mbs, c2mbs)
    //List(c1mbs, c2mbs)
    interface {
      print(f"energy = ${energy(bodies)}; d_c1c2 = ${c1.coord.dist(c2.coord)}%.2f", 20, 20, WHITE)
      //print(f"energy = $energy", 20, 20, WHITE)
    }
  }

  def test2() {
    val c1 = new MyCircle2(nextId, DVec(w,h), randomSpeed, 5, 1f, this)
    val c1mbs = c1.currentState.toMutableBodyState
    system_evolution.addBody(c1mbs, (time, helper) => DVec(0, -9.81*c1mbs.mass), (time, helper) => 0.0)
    val c2 = new MyCircle2(nextId, DVec(w+50,h), randomSpeed, 5, 1f, this)
    val c2mbs = c2.currentState.toMutableBodyState
    system_evolution.addBody(c2mbs, (time, helper) => DVec(0, -9.81*c2mbs.mass), (time, helper) => 0.0)
    system_evolution.addJoint(c1mbs, DVec(45, 0), c2mbs, DVec(-5, 0))
    system_evolution.addJoint(c1mbs, DVec(0, 5), w2mbs, DVec(0, -95))
    val bodies = List(c1mbs, c2mbs)
    render {
      drawLine(c1mbs.coord, c2mbs.coord, WHITE)
      drawLine(c1mbs.coord, w2mbs.coord, WHITE)
    }
    interface {
      print(f"energy = ${energy(bodies)}; d_c1c2 = ${c1.coord.dist(c2.coord)}%.2f; d_c1w2 = ${c1.coord.dist(w2mbs.coord)}%.2f", 20, 20, WHITE)
      //print(f"energy = $energy", 20, 20, WHITE)
    }
  }

  test1()

  private def nextStep() {
    (1 to dt).foreach(x => system_evolution.step())
  }
  nextStep()

  private var _center = windowCenter

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL)) stopApp())

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center += Vec(5/globalScale, 0)})

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {dt += 1})
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {if(dt > 1)dt -= 1})

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01f) {
      if(globalScale > 1) globalScale -= 1
      else if(globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
      if(globalScale < 0.01f) globalScale = 0.01f
    }
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 5) {
      if(globalScale < 1) globalScale += 0.1f
      else globalScale += 1
    }
  })

  action {
    nextStep()
  }

  center = _center
  interface {
    print(s"time x$dt", windowWidth-20, 20, WHITE, align = "bottom-right")
  }
}
