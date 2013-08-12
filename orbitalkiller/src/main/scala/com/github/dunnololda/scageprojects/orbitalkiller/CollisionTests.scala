package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable



object CollisionTests extends ScageScreenApp("Collision Tests", 800, 600){
  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    1,
    force = (time, bs, other_bodies) => {
      Vec.zero
    },
    torque = (time, bs, other_bodies) => {
      0f
    })((time, body_states))

  val c1 = new MyCircle("c1", Vec(windowWidth/2-300, windowHeight/2), Vec(0.5f, 0), 30)
  val c2 = new MyCircle("c2", Vec(windowWidth/2+300, windowHeight/2), Vec(-0.5f, 0), 30)

  private val real_system_evolution =
    futureSystemEvolutionFrom(0, List(c1.currentState, c2.currentState)).iterator

  private def nextStep() {
    val (t, body_states) = real_system_evolution.next()
    body_states.foreach {
      case bs => current_body_states(bs.index) = bs
    }
  }
  nextStep()

  action {
    nextStep()
    if(circleCircleCollision(c1.shape, c2.shape).nonEmpty) pause()
  }

  render(10) {
    circleCircleCollision(c1.shape, c2.shape).foreach(p => drawFilledCircle(p.contact_point, 3, RED))
  }
}

import CollisionTests._

class MyCircle(val index:String, init_coord:Vec, init_velocity:Vec, val radius:Float) {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = 1,
      I = 0f,
      force = Vec.zero,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      torque = 0f,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      elasticity = 1f,
      shape = (coord, rotation) => CircleShape(coord, radius)))

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  def shape:CircleShape = CircleShape(coord, radius)

  render(0) {
    drawCircle(coord, radius, WHITE)
  }
}


