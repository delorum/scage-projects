package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.tests.BodyStatesHolder._

import scala.collection.mutable.ArrayBuffer

object ConstraintsTest extends ScageScreenAppD("Constraints Test", 640, 480) {
  val base_dt = 1.0/63.0
  val dt = base_dt
  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    dt = dt, maxMultiplier = 1, base_dt = base_dt,
    force = (time, bs, other_bodies) => {
      DVec.zero/*DVec(0, -9.81*bs.mass)*/
    },
    torque = (time, bs, other_bodies) => {
      0f
    }, changeFunction = (tacts, body_states) => {
      val mutable_body_states = body_states.map(_.toMutableBodyState)
      for {
        c1 <- mutable_body_states.find(_.body.index == "c1")
        c2 <- mutable_body_states.find(_.body.index == "c2")
      } {
        val axis = c2.coord - c1.coord
        val currentDistance = axis.norma
        val unitAxis = axis.n
        val relVel = (c2.vel - c1.vel)*unitAxis
        val relDist = currentDistance - 80
        val remove = relVel+relDist/dt
        val impulse = remove/(c1.body.invMass + c2.body.invMass)
        val I = unitAxis * impulse
        c1.applyImpulse(I, unitAxis)
        c2.applyImpulse(-I, unitAxis)
      }

      (tacts, mutable_body_states.map(_.toImmutableBodyState))
    })((time, body_states))

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(-1 + math.random*2, -1 + math.random*2).n*50f

  def addCircleBody(i:Int) {
    val c =  new MyCircle2(s"c$i", randomPos, randomSpeed, 5, 1f, this)
    /*if(dynamic_bodies.forall(b => maybeCollision(b.currentState, c.currentState).isEmpty) &&
      !coordOnRectCentered(c.coord, Vec(w+60, h-20), 30, 20) &&
      !coordOnRectCentered(c.coord, Vec(w-60, h-20), 30, 20))
    {*/
    dynamic_bodies += c
    //} else addCircleBody(i)
  }

  val dynamic_bodies = ArrayBuffer[MyBody]()

  val w1 = new MyWall2("w1", Vec(w-100, h-100),  Vec(w-100, h+100), this)
  val w2 = new MyWall2("w2", Vec(w-100, h+100),  Vec(w+100, h+100), this)
  val w3 = new MyWall2("w3", Vec(w+100, h+100),  Vec(w+100, h-100), this)
  val w4 = new MyWall2("w4", Vec(w+100, h-100),  Vec(w-100, h-100), this)

  addCircleBody(1)
  addCircleBody(2)

  private val real_system_evolution =
    futureSystemEvolutionFrom(0, dynamic_bodies.map(_.currentState).toList ::: List(
      w1.currentState,
      w2.currentState,
      w3.currentState,
      w4.currentState)).iterator

  private def nextStep() {
    val (_, body_states) = real_system_evolution.next()
    body_states.foreach {
      case bs => current_body_states(bs.index) = bs
    }
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
    print(s"energy = $energy", 20, 20, WHITE)
  }
}
