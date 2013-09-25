package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable
import scala.collection.mutable.ArrayBuffer


object CollisionTests extends ScageScreenApp("Collision Tests", 640, 480){
  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    dt = 1, elasticity = 0.999f,
    force = (time, bs, other_bodies) => {
      /*Vec(0, -0.01f) + */Vec.zero
    },
    torque = (time, bs, other_bodies) => {
      0f
    })((time, body_states))

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(math.random, math.random).n*0.5f

  val dynamic_bodies = ArrayBuffer[MyBody]()
  val b1 = new MyBox("b1", Vec(w+60, h), Vec(-0.3f, 0), 30, 20, 1f*6)
  dynamic_bodies += b1
  val b2 = new MyBox("b2", Vec(w-60, h), Vec(0.3f, 0), 30, 20, 1f*6)
  dynamic_bodies += b2
  /*val p1 = new MyPentagon("p1", Vec(w-60, h-20), Vec(0.0f, 0), 20, 1f*6)
  dynamic_bodies += p1*/
  def addCircleBody(i:Int) {
    val c =  new MyCircle(s"c$i", randomPos, randomSpeed, 5, 1f)
    if(dynamic_bodies.forall(b => maybeCollision(b.currentState, c.currentState).isEmpty) &&
      !coordOnRectCentered(c.coord, Vec(w+60, h-20), 30, 20) &&
      !coordOnRectCentered(c.coord, Vec(w-60, h-20), 30, 20))
    {
      dynamic_bodies += c
    } else addCircleBody(i)
  }
  //(1 to 20).map(i => addCircleBody(i))
  /*val c1 = new MyCircle("c1", Vec(w, h), Vec(0.0f, -0.0f), 5, 1f)
  dynamic_bodies += c1*/
  val l1 = new MyLine("l1", Vec(w-30, h-30), Vec(w+30, h+30))
  dynamic_bodies += l1

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
    val spaces = splitSpace(new Space(current_body_states.values.toList, Vec.zero), 5, 3)
    spaces.foreach {
      case s =>
        drawRectCentered(s.center, s.width, s.height, WHITE)
        print(s.bodies.length, s.center, max_font_size/globalScale, WHITE, align = "center")
    }
  }

  interface {
    print(s"$energy", 20, 20, WHITE)
  }
}

import CollisionTests._

sealed trait MyBody {
  def currentState:BodyState
}

class MyPentagon(val index:String, init_coord:Vec, init_velocity:Vec, val len:Float, val mass:Float) extends MyBody {
  def currentState: BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = {
        val one = Vec(0, len).rotateDeg(0)
        val two = Vec(0, len).rotateDeg(0+72)
        val three = Vec(0, len).rotateDeg(0+72+72)
        val four = Vec(0, len).rotateDeg(0+72+72+72)
        val five = Vec(0, len).rotateDeg(0+72+72+72+72)
        PolygonShape(List(one, two, three, four, five))
      },
      is_static = false))

  render {
    val state = currentState
    val coord = state.coord
    val rotation = state.ang
    val one = coord + Vec(0, len).rotateDeg(rotation)
    val two = coord + Vec(0, len).rotateDeg(rotation+72)
    val three = coord + Vec(0, len).rotateDeg(rotation+72+72)
    val four = coord + Vec(0, len).rotateDeg(rotation+72+72+72)
    val five = coord + Vec(0, len).rotateDeg(rotation+72+72+72+72)
    val color = GREEN
    drawSlidingLines(List(one, two, three, four, five, one), color)

    val AABB(c, w, h) = state.aabb
    drawRectCentered(c, w, h, GREEN)
  }
}

class MyCircle(val index:String, init_coord:Vec, init_velocity:Vec, val radius:Float, val mass:Float) extends MyBody {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = CircleShape(radius),
      is_static = false))

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  render(0) {
    val color = GREEN
    drawCircle(coord, radius, color)
    drawLine(coord, coord + Vec(0,1).rotateDeg(currentState.ang).n*radius, color)
    val AABB(c, w, h) = currentState.aabb
    drawRectCentered(c, w, h, color)
  }
}

class MyBox(val index:String, init_coord:Vec, init_velocity:Vec, val w:Float, val h:Float, val mass:Float) extends MyBody {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = BoxShape(w, h),
      is_static = false))

  render(0) {
    val color = GREEN
    openglLocalTransform {
      openglMove(currentState.coord)
      openglRotateDeg(currentState.ang)
      drawRectCentered(Vec.zero, w, h, color)
    }

    val AABB(c, w2, h2) = currentState.aabb
    drawRectCentered(c, w2, h2, color)
  }
}

class MyWall(index:String, from:Vec, to:Vec) extends MyBody {
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = -1,  // infinite mass
      acc = Vec.zero,
      vel = Vec.zero,
      coord = from,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = LineShape(to-from),
      is_static = true))

  render(0) {
    val color = GREEN
    drawLine(from, to, color)
    val AABB(c, w, h) = currentState.aabb
    drawRectCentered(c, w, h, color)
  }
}

class MyLine(index:String, from:Vec, to:Vec) extends MyBody {
  private val vec = to - from
  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = 1,  // infinite mass
      acc = Vec.zero,
      vel = Vec.zero,
      coord = from,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = LineShape(to-from),
      is_static = false))

  render(0) {
    val color = GREEN
    val state = currentState
    drawLine(state.coord, state.coord + vec.rotateDeg(state.ang), color)
    val AABB(c, w, h) = currentState.aabb
    drawRectCentered(c, w, h, color)
  }
}


