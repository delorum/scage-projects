package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.tests.BodyStatesHolder._

import scala.collection.mutable.ArrayBuffer

import com.github.dunnololda.scageprojects.orbitalkiller.tests.BodyStatesHolder._

object ConstraintsTest extends ScageScreenAppD("Constraints Test", 640, 480) {
  val base_dt = 1.0/63.0
  val dt = base_dt

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(-1 + math.random*2, -1 + math.random*2).n*50f

  def addCircleBody(i:Int) = {
    val c =  new MyCircle2(nextId, randomPos, randomSpeed, 5, 1f, this)
    /*if(dynamic_bodies.forall(b => maybeCollision(b.currentState, c.currentState).isEmpty) &&
      !coordOnRectCentered(c.coord, Vec(w+60, h-20), 30, 20) &&
      !coordOnRectCentered(c.coord, Vec(w-60, h-20), 30, 20))
    {*/
    dynamic_bodies += c
    //} else addCircleBody(i)
    c
  }

  val dynamic_bodies = ArrayBuffer[MyBody]()

  val w1 = new MyWall2(nextId, Vec(w-100, h-100),  Vec(w-100, h+100), this)
  system_evolution.addBody(w1.currentState.toMutableBodyState)
  val w2 = new MyWall2(nextId, Vec(w-100, h+100),  Vec(w+100, h+100), this)
  system_evolution.addBody(w2.currentState.toMutableBodyState)
  val w3 = new MyWall2(nextId, Vec(w+100, h+100),  Vec(w+100, h-100), this)
  system_evolution.addBody(w3.currentState.toMutableBodyState)
  val w4 = new MyWall2(nextId, Vec(w+100, h-100),  Vec(w-100, h-100), this)
  system_evolution.addBody(w4.currentState.toMutableBodyState)

  /*val c1 = addCircleBody(1)
  val c2 = addCircleBody(2)*/
  val c1 = new MyCircle2(nextId, DVec(w,h), DVec(0, -30), 5, 1f, this)
  val c1mbs = c1.currentState.toMutableBodyState
  system_evolution.addBody(c1mbs)
  val c2 = new MyCircle2(nextId, DVec(w+80,h+80), DVec(0, -30), 5, 1f, this)
  val c2mbs = c2.currentState.toMutableBodyState
  system_evolution.addBody(c2mbs)
  dynamic_bodies += c1 += c2

  system_evolution.addJoint(Joint(c1mbs, DVec(0, 5), c2mbs, DVec(0, 5)))

  private def nextStep() {
    system_evolution.step()
  }
  nextStep()

  private var _center = windowCenter

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL)) stopApp())

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center += Vec(5/globalScale, 0)})

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

  def energy =
    dynamic_bodies.map(b1 => {
      b1.currentState.mass*b1.currentState.vel.norma2/2f +
      b1.currentState.I*b1.currentState.ang_vel.toRad*b1.currentState.ang_vel.toRad/2f/* +
      b1.currentState.mass*9.81*b1.currentState.coord.y*/
    }).sum

  interface {
    print(f"energy = $energy; distance = ${c1.coord.dist(c2.coord)}%.2f", 20, 20, WHITE)
  }
}
