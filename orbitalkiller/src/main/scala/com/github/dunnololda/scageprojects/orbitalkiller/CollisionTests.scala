package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable

sealed trait Shape {
  def collisions(other_shape:Shape):List[Vec]
}

case class CircleShape(center:Vec, radius:Float) extends Shape {
  def collisions(other_shape: Shape): List[Vec] = {
    other_shape match {
      case CircleShape(Vec(x2, y2), r2) =>
        val Vec(x1, y1) = center
        val r1 = radius
        val d = math.sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)).toFloat
        if(d < r1+r2 && d >= math.abs(r1-r2)) {
          val b = (r2*r2 - r1*r1)/(2*d)
          val a = d - b
          val h = math.sqrt(r2*r2 - b*b).toFloat

          val x = x1 + (x2-x1)/(d/a)
          val y = x1 + (x2-x1)/(d/a)

          val x3 = x - (y-y2)*h/b
          val y3 = y + (x-x2)*h/b
          val x4 = x+(y-y2)*h/b
          val y4 = y-(x-x2)*h/b

          List(Vec(x3, y3), Vec(x4, y4))
        } else if(d == r1+r2) {

          val b = (r2*r2 - r1*r1)/(2*d)
          val a = d - b

          val x = x1 + (x2-x1)/(d/a)
          val y = x1 + (x2-x1)/(d/a)

          List(Vec(x, y))
        } else Nil
      case _ => Nil
    }
  }
}

object CollisionTests extends ScageScreenApp("Collision Tests", 800, 600){
  def circleCircleCollision(c1:CircleShape, c2:CircleShape):Option[Vec] = {
    if (math.abs(c1.center.dist2(c2.center) - (c1.radius+c2.radius)*(c1.radius+c2.radius)) < 1f) {
      Some(c1.center + (c2.center - c1.center).n*c1.radius)
    } else None
  }

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
    circleCircleCollision(c1.shape, c2.shape).foreach(p => drawFilledCircle(p, 3, RED))
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
      ang = 0f))

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  def shape:CircleShape = CircleShape(coord, radius)

  render(0) {
    drawCircle(coord, radius, WHITE)
  }
}


