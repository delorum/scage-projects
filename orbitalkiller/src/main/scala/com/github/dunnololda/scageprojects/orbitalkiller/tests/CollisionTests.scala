package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._
import collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.AABB
import com.github.dunnololda.scageprojects.orbitalkiller.LineShape
import com.github.dunnololda.scageprojects.orbitalkiller.CircleShape
import com.github.dunnololda.scageprojects.orbitalkiller.BoxShape
import com.github.dunnololda.scageprojects.orbitalkiller.BodyState
import com.github.dunnololda.scageprojects.orbitalkiller.PolygonShape


object CollisionTests extends ScageScreenApp("Collision Tests", 640, 480){
  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    dt = 1.0/63.0, maxMultiplier = 1, base_dt = 1.0/63.0,
    force = (time, bs, other_bodies) => {
      /*Vec(0, -0.01f) + */DVec.zero/*DVec(0, -9.81*bs.mass)*/
    },
    torque = (time, bs, other_bodies) => {
      0f
    })((time, body_states))

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(-1 + math.random*2, -1 + math.random*2).n*50f

  val dynamic_bodies = ArrayBuffer[MyBody]()
  val b1 = new MyBox("b1", Vec(w-60, h), Vec(0.3f, 0), 30, 20, 1)
  dynamic_bodies += b1
  /*val b2 = new MyBox("b2", Vec(w+60, h), Vec(-0.3f, 0), 30, 20, 1f*6)
  dynamic_bodies += b2*/
  val p1 = new MyPentagon("p1", Vec(w-60, h-40), Vec(0.0f, 0), 20, 1)
  dynamic_bodies += p1
  def addCircleBody(i:Int) {
    val c =  new MyCircle(s"c$i", randomPos, randomSpeed, 5, 1f)
    /*if(dynamic_bodies.forall(b => maybeCollision(b.currentState, c.currentState).isEmpty) &&
      !coordOnRectCentered(c.coord, Vec(w+60, h-20), 30, 20) &&
      !coordOnRectCentered(c.coord, Vec(w-60, h-20), 30, 20))
    {*/
      dynamic_bodies += c
    //} else addCircleBody(i)
  }
  (1 to 40).foreach(i => addCircleBody(i))
  /*val c1 = new MyCircle("c1", Vec(w, h), Vec(0.0f, -0.0f), 5, 1f)
  dynamic_bodies += c1*/

  /*val c1 = new MyCircle("c1", Vec(w-60, h), Vec(0.0f, 0), 30, 1f)*/
  /*val c2 = new MyCircle("c2", Vec(w+60, h+5), Vec(-0.5f, 0), 1f, 30)*/
  /*val b1 = new MyBox("b1", Vec(w+60, h-20), Vec(-0.0f, 0), 30, 20, 1f)*/
  val w1 = new MyWall("w1", Vec(w-100, h-100),  Vec(w-100, h+100))
  val w2 = new MyWall("w2", Vec(w-100, h+100),  Vec(w+100, h+100))
  val w3 = new MyWall("w3", Vec(w+100, h+100),  Vec(w+100, h-100))
  val w4 = new MyWall("w4", Vec(w+100, h-100),  Vec(w-100, h-100))

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

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center += Vec(5/globalScale, 0)})

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL)) stopApp())

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

  /*println(s"${w-100} : ${w+100}")
  println(s"${h-100} : ${h+100}")
  private def outOfArea(coord:Vec):Boolean = {
    coord.x < w-100 ||
    coord.x > w+100 ||
    coord.y < h-100 ||
    coord.y > h+100
  }

  action {
    if(currentBodyStates.exists(bs => outOfArea(bs.coord))) {
      pause()
      currentBodyStates.foreach(bs => {
        if(outOfArea(bs.coord)) println(s"!!!out!!! ${bs.index} : ${bs.coord} : ${bs.vel} : ${bs.ang} : ${bs.ang_vel}")
        else println(s"${bs.index} : ${bs.coord} : ${bs.vel} : ${bs.ang} : ${bs.ang_vel}")
      })
    }
  }

  private def insideBox(index:String, coord:Vec):Boolean = {
    index != b1.index && b1.currentState.coord.dist(coord) < math.min(b1.w, b1.h)
  }

  action {
    if(currentBodyStates.exists(bs => insideBox(bs.index, bs.coord))) {
      pause()
      currentBodyStates.foreach(bs => {
        if(insideBox(bs.index, bs.coord)) println(s"!!!box!!! ${bs.index} : ${bs.coord} : ${bs.vel} : ${bs.ang} : ${bs.ang_vel}")
        else println(s"${bs.index} : ${bs.coord} : ${bs.vel} : ${bs.ang} : ${bs.ang_vel}")
      })
    }
  }

  private def insidePentagon(index:String, coord:Vec):Boolean = {
    index != p1.index && p1.currentState.coord.dist(coord) < p1.len*0.8f
  }

  action {
    if(currentBodyStates.exists(bs => insidePentagon(bs.index, bs.coord))) {
      pause()
      currentBodyStates.foreach(bs => {
        if(insidePentagon(bs.index, bs.coord)) println(s"!!!pentagon!!! ${bs.index} : ${bs.coord} : ${bs.vel} : ${bs.ang} : ${bs.ang_vel}")
        else println(s"${bs.index} : ${bs.coord} : ${bs.vel} : ${bs.ang} : ${bs.ang_vel}")
      })
    }
  }*/

  def energy =
    dynamic_bodies.map(b1 => {
      b1.currentState.mass*b1.currentState.vel.norma2/2f +
        b1.currentState.I*b1.currentState.ang_vel.toRad*b1.currentState.ang_vel.toRad/2f
    }).sum

  center = _center

  render {
    val spaces = splitSpace(new Space(current_body_states.values.toList.map(_.mutableBodyState), DVec.dzero), 5, 3)
    spaces.foreach {
      case s =>
        drawRectCentered(s.center.toVec, s.width.toFloat, s.height.toFloat, WHITE)
        print(s.bodies.length, s.center.toVec, max_font_size/globalScale, WHITE, align = "center")
    }
  }

  interface {
    print(s"$energy", 20, 20, WHITE)
  }
}

import CollisionTests._

trait MyBody {
  def currentState:BodyState
}

class MyPentagon(val index:String, init_coord:Vec, init_velocity:Vec, val len:Double, val mass:Double) extends MyBody {
  def currentState: BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = {
        val one   = DVec(0, len).rotateDeg(0)
        val two   = DVec(0, len).rotateDeg(0+72)
        val three = DVec(0, len).rotateDeg(0+72+72)
        val four  = DVec(0, len).rotateDeg(0+72+72+72)
        val five  = DVec(0, len).rotateDeg(0+72+72+72+72)
        PolygonShape(List(one, two, three, four, five))
      },
      is_static = false, restitution = 1, staticFriction = 0, dynamicFriction = 0))

  render {
    val state = currentState
    val coord = state.coord
    val rotation = state.ang
    val one = coord + DVec(0, len).rotateDeg(rotation)
    val two = coord + DVec(0, len).rotateDeg(rotation+72)
    val three = coord + DVec(0, len).rotateDeg(rotation+72+72)
    val four = coord + DVec(0, len).rotateDeg(rotation+72+72+72)
    val five = coord + DVec(0, len).rotateDeg(rotation+72+72+72+72)
    val color = WHITE
    drawSlidingLines(List(one, two, three, four, five, one).map(_.toVec), color)

    /*val AABB(c, w, h) = state.aabb
    drawRectCentered(c.toVec, w.toFloat, h.toFloat, WHITE)*/
  }
}

class MyCircle(val index:String, init_coord:DVec, init_velocity:DVec, val radius:Double, val mass:Double) extends MyBody {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = CircleShape(radius),
      is_static = false, restitution = 1, staticFriction = 0, dynamicFriction = 0))

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  render(0) {
    val color = WHITE
    drawCircle(coord.toVec, radius.toFloat, color)
    drawLine(coord.toVec, (coord + DVec(0,1).rotateDeg(currentState.ang).n*radius).toVec, color)
    /*val AABB(c, w, h) = currentState.aabb
    drawRectCentered(c.toVec, w.toFloat, h.toFloat, color)*/
  }
}

class MyBox(val index:String, init_coord:DVec, init_velocity:DVec, val w:Double, val h:Double, val mass:Double) extends MyBody {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0.0,
      ang_vel = 0.0,
      ang = 0.0,
      shape = BoxShape(w, h),
      is_static = false, restitution = 1, staticFriction = 0, dynamicFriction = 0))

  render(0) {
    val color = WHITE
    openglLocalTransform {
      openglMove(currentState.coord.toVec)
      openglRotateDeg(currentState.ang.toFloat)
      drawRectCentered(Vec.zero, w.toFloat, h.toFloat, color)
    }

    /*val AABB(c, w2, h2) = currentState.aabb
    drawRectCentered(c.toVec, w2.toFloat, h2.toFloat, color)*/
  }
}

class MyWall(index:String, from:DVec, to:DVec) extends MyBody {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = -1,  // infinite mass
      acc = DVec.dzero,
      vel = DVec.dzero,
      coord = from,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = LineShape(to-from),
      is_static = true, restitution = 1, staticFriction = 0, dynamicFriction = 0))

  render(0) {
    val color = WHITE
    drawLine(from.toVec, to.toVec, color)
    /*val AABB(c, w, h) = currentState.aabb
    drawRectCentered(c.toVec, w.toFloat, h.toFloat, color)*/
  }
}


