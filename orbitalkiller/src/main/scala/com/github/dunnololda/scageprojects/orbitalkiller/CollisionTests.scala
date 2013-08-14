package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable



object CollisionTests extends ScageScreenApp("Collision Tests", 640, 480){
  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    1,
    force = (time, bs, other_bodies) => {
      bs.index match {
        case "b1" =>
          bs.currentShape match {
            case b:BoxShape =>
              if(_force) Vec(1, 0).rotateDeg(bs.ang)
              else Vec.zero
            case _ => Vec.zero
          }
        case _ => Vec.zero
      }
    },
    torque = (time, bs, other_bodies) => {
      bs.index match {
        case "b1" =>
          bs.currentShape match {
            case b:BoxShape =>
              if(_force) {
                val force = Vec(1, 0)
                val position = Vec(-b.w/2, b.h/4)
                -force*/position
              } else 0f
            case _ => 0f
          }
        case _ => 0f
      }
    })((time, body_states))

  val w = windowWidth/2
  val h = windowHeight/2

  def randomSpeed = Vec(math.random, math.random).n*1f

  val c1 = new MyCircle("c1", Vec(w-60, h), Vec(0.5f, 0), 30, 1f)
  /*val c2 = new MyCircle("c2", Vec(w+60, h+5), Vec(-0.5f, 0), 1f, 30)*/
  val b1 = new MyBox("b1", Vec(w+60, h-20), Vec(-0.5f, 0), 30, 20, 1f)
  val w1 = new MyWall("w1", Vec(w-100, h-100),  Vec(w-100, h+100))
  val w2 = new MyWall("w2", Vec(w-100, h+100),  Vec(w+100, h+100))
  val w3 = new MyWall("w3", Vec(w+100, h+100),  Vec(w+100, h-100))
  val w4 = new MyWall("w4", Vec(w+100, h-100),  Vec(w-100, h-100))

  private val real_system_evolution =
    futureSystemEvolutionFrom(0, List(
      c1.currentState,
      b1.currentState,
      w1.currentState,
      w2.currentState,
      w3.currentState,
      w4.currentState)).iterator

  private def nextStep() {
    val (t, body_states) = real_system_evolution.next()
    body_states.foreach {
      case bs => current_body_states(bs.index) = bs
    }
  }
  nextStep()

  private var _force = false
  key(KEY_7, onKeyDown = {
    _force = true
  }, onKeyUp = {
    _force = false
  })

  action {
    nextStep()
  }

  def energy =
    c1.mass*c1.currentState.vel*c1.currentState.vel/2f +
    c1.currentState.I*c1.currentState.ang_vel*c1.currentState.ang_vel/2f +
    b1.mass*b1.currentState.vel*b1.currentState.vel/2f +
    b1.currentState.I*b1.currentState.ang_vel*b1.currentState.ang_vel/2f

  interface {
    print(energy, 20, 20, WHITE)
  }
}

import CollisionTests._

class MyCircle(val index:String, init_coord:Vec, init_velocity:Vec, val radius:Float, val mass:Float) {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      I = mass*radius*radius/2f,
      force = Vec.zero,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      torque = 0f,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      elasticity = 1f,
      shape = (coord, rotation) => CircleShape(coord, radius),
      is_static = false))

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  def shape:CircleShape = CircleShape(coord, radius)

  render(0) {
    drawCircle(coord, radius, WHITE)
    drawLine(coord, coord + Vec(0,1).rotateDeg(currentState.ang).n*radius, WHITE)
  }
}

class MyBox(val index:String, init_coord:Vec, init_velocity:Vec, val w:Float, val h:Float, val mass:Float) {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      I = mass*(w*w + h*h)/12f,
      force = Vec.zero,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      torque = 0f,
      ang_acc = 0f,
      ang_vel = 1f,
      ang = 0f,
      elasticity = 1f,
      shape = (coord, rotation) => {
        BoxShape(coord, w, h, rotation)
      },
      is_static = false))

  render(0) {
    openglMove(currentState.coord)
    openglRotateDeg(currentState.ang)
    drawRectCentered(Vec.zero, w, h, WHITE)
  }
}

class MyWall(index:String, from:Vec, to:Vec) {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = -1,  // infinite mass
      I = 0f,
      force = Vec.zero,
      acc = Vec.zero,
      vel = Vec.zero,
      coord = Vec.zero,
      torque = 0f,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      elasticity = 1f,
      shape = (coord, rotation) => LineShape(from, to),
      is_static = true))

  render(0) {
    drawLine(from, to, WHITE)
  }
}


