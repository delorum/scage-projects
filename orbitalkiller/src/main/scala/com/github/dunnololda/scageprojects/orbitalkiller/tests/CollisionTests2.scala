package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CollisionTests2 extends ScageScreenAppD("Collision Tests 2", 640, 480) {
  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    dt = 1.0/63.0, base_dt = 1.0/63.0, elasticity = 0.3,
    force = (time, bs, other_bodies) => {
      DVec(0, -9.81*bs.mass)
    },
    torque = (time, bs, other_bodies) => {
      0f
    })((time, body_states))

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(math.random, math.random).n*0.5f

  val dynamic_bodies = ArrayBuffer[MyBody]()

  def addCircleBody(i:Int) {
    val c =  new MyCircle2(s"c$i", randomPos, randomSpeed, 5, 1f)
    /*if(dynamic_bodies.forall(b => maybeCollision(b.currentState, c.currentState).isEmpty) &&
      !coordOnRectCentered(c.coord, Vec(w+60, h-20), 30, 20) &&
      !coordOnRectCentered(c.coord, Vec(w-60, h-20), 30, 20))
    {*/
      dynamic_bodies += c
    //} else addCircleBody(i)
  }

  val w1 = new MyWall2("w1", Vec(w-100, h-100),  Vec(w-100, h+100))
  val w2 = new MyWall2("w2", Vec(w-100, h+100),  Vec(w+100, h+100))
  val w3 = new MyWall2("w3", Vec(w+100, h+100),  Vec(w+100, h-100))
  val w4 = new MyWall2("w4", Vec(w+100, h-100),  Vec(w-100, h-100))

  val b1 = new MyBox2("b1", Vec(w-60, h), Vec(0.0f, 0), 30, 20, 1)
  dynamic_bodies += b1
  val p1 = new MyPentagon2("p1", Vec(w-60, h-40), Vec(0.0f, 0), 20, 1)
  dynamic_bodies += p1

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

  def energy =
    dynamic_bodies.map(b1 => {
      b1.currentState.mass*b1.currentState.vel.norma2/2f +
        b1.currentState.I*b1.currentState.ang_vel.toRad*b1.currentState.ang_vel.toRad/2f
    }).sum

  center = _center

  /*render {
    /*val spaces = splitSpace(new Space(current_body_states.values.toList, DVec.dzero), 5, 3)
    spaces.foreach {
      case s =>
        drawRectCentered(s.center.toVec, s.width.toFloat, s.height.toFloat, WHITE)
        print(s.bodies.length, s.center.toVec, max_font_size/globalScale, WHITE, align = "center")
    }*/
  }*/

  /*interface {
    //print(s"$energy", 20, 20, WHITE)
    println("=================================")
    currentBodyStates.foreach {
      case bs =>
        /*if(bs.collisions.nonEmpty) {
          bs.collisions.foreach {
            case c =>
              println(s"${bs.index} has collision with ${c.collided_body.index} in point ${c.contact_point} with separation ${c.separation}")
          }
        }*/
    }
    println("=================================")
  }*/
}

import CollisionTests2._

class MyPentagon2(val index:String, init_coord:Vec, init_velocity:Vec, val len:Double, val mass:Double) extends MyBody {
  def currentState: BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass = mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 10f,
      shape = {
        val one   = DVec(0, len).rotateDeg(0)
        val two   = DVec(0, len).rotateDeg(0+72)
        val three = DVec(0, len).rotateDeg(0+72+72)
        val four  = DVec(0, len).rotateDeg(0+72+72+72)
        val five  = DVec(0, len).rotateDeg(0+72+72+72+72)
        PolygonShape(List(one, two, three, four, five))
      },
      is_static = false))

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
    drawSlidingLines(List(one, two, three, four, five, one), color)

    /*state.collisions.foreach {
      case CollisionData(other_body, contact, normal, separation, contacts, _, _) =>
        contacts.foreach {
          case c =>
            drawFilledCircle(c.getPosition.toDVec, 3, GREEN)
            drawLine(c.getPosition.toDVec, c.getPosition.toDVec + c.getNormal.toDVec.n*10, GREEN)
            print(f"${c.getSeparation}%.2f", c.getPosition.toVec + c.getNormal.toVec.n*10, WHITE)
        }
    }*/
    /*if(state.collisions.nonEmpty) {
      println(state.collisions.flatMap(c => c.contacts.map(cc => f"${cc.getSeparation}%.2f")).mkString(" : "))
    }*/

    /*val AABB(c, w, h) = state.aabb
    drawRectCentered(c.toVec, w.toFloat, h.toFloat, GREEN)*/
  }
}

class MyCircle2(val index:String, init_coord:DVec, init_velocity:DVec, val radius:Double, val mass:Double) extends MyBody {
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
      is_static = false))

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

class MyBox2(val index:String, init_coord:DVec, init_velocity:DVec, val w:Double, val h:Double, val mass:Double) extends MyBody {
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
      is_static = false))

  render(0) {
    val state = currentState
    val color = WHITE
    openglLocalTransform {
      openglMove(state.coord.toVec)
      openglRotateDeg(state.ang.toFloat)
      drawRectCentered(Vec.zero, w.toFloat, h.toFloat, color)
    }

    /*if(state.collisions.nonEmpty) {
      println(state.collisions.flatMap(c => {
        c.contacts.map(cc => {
          f"${cc.getSeparation}%.2f"
        })
      }).mkString(" : "))
    }*/

    /*state.collisions.foreach {
      case CollisionData(other_body, contact, normal, separation, contacts, _, _) =>
        contacts.foreach {
          case c =>
            drawFilledCircle(c.getPosition.toDVec, 3, GREEN)
            drawLine(c.getPosition.toDVec, c.getPosition.toDVec + c.getNormal.toDVec.n*10, GREEN)
            print(f"$separation%.2f", c.getPosition.toVec + c.getNormal.toVec.n*10, WHITE)
        }
    }*/

    /*val AABB(c, w2, h2) = currentState.aabb
    drawRectCentered(c.toVec, w2.toFloat, h2.toFloat, color)*/
  }
}

class MyWall2(index:String, from:DVec, to:DVec) extends MyBody {
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
      is_static = true))

  render(0) {
    val color = WHITE
    drawLine(from.toVec, to.toVec, color)
    /*val AABB(c, w, h) = currentState.aabb
    drawRectCentered(c.toVec, w.toFloat, h.toFloat, color)*/
  }
}
