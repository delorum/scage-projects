package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._

import scala.collection.mutable.ArrayBuffer

object MultipleShapesTest extends ScageScreenAppD("Multiple Shapes Test", 640, 480) {
  val b1 = new MutableBodyState(BodyState(
    index = "b1",
    mass = 1,
    coord = DVec(100, 320),
    vel = DVec(20,0),
    shape = PolygonShape(List(DVec(-10, 10), DVec(-10, 20), DVec(0, 20), DVec(10,10)), Nil),
    restitution = 0.9
  ))

  val b2 = new MutableBodyState(BodyState(
    index = "b2",
    mass = 1,
    coord = DVec(540, 323),
    vel = DVec(-20,0),
    shape = PolygonShape(List(DVec(-10, 10), DVec(0, 20), DVec(10,10)), Nil),
    restitution = 0.9
  ))

  val mutable_system = OrbitalKiller.makeThisAndOthers(ArrayBuffer(b1, b2))

  key(KEY_Q, onKeyDown = if(keyPressed(KEY_RCONTROL) || keyPressed(KEY_LCONTROL)) stopApp())

  action {
    mutableSystemEvolution(mutable_system, /*1, */1.0/63)
  }

  render {
    openglLocalTransform {
      openglMove(b1.coord)
      openglRotate(b1.ang)
      drawPolygon(b1.shape.asInstanceOf[PolygonShape].points, WHITE)
    }
    openglLocalTransform {
      openglMove(b2.coord)
      openglRotate(b2.ang)
      drawPolygon(b2.shape.asInstanceOf[PolygonShape].points, WHITE)
    }
    drawFilledCircle(b1.coord, 3, WHITE)
    drawFilledCircle(b2.coord, 3, WHITE)
  }
}
