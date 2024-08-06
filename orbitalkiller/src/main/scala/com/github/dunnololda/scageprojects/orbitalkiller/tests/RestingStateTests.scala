package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.{BoxShape, LineShape}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.BodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.stream.SystemEvolutionStream

object RestingStateTests extends ScageScreenAppD("Resting State Tests", 800, 600) {

  val w1 = BodyState(
    nextId,
    1,
    DVec.zero,
    DVec.zero,
    DVec(0, 0),
    0.0,
    0.0,
    0.0,
    LineShape(DVec(windowWidth, 0)),
    is_static = true
  )

  val w2 = BodyState(
    nextId,
    1,
    DVec.zero,
    DVec.zero,
    DVec(0, windowHeight),
    0.0,
    0.0,
    0.0,
    LineShape(DVec(windowWidth, 0)),
    is_static = true
  )

  val w3 = BodyState(
    nextId,
    1,
    DVec.zero,
    DVec.zero,
    DVec(0, 0),
    0.0,
    0.0,
    0.0,
    LineShape(DVec(0, windowHeight)),
    is_static = true
  )

  val w4 = BodyState(
    nextId,
    1,
    DVec.zero,
    DVec.zero,
    DVec(windowWidth, 0),
    0.0,
    0.0,
    0.0,
    LineShape(DVec(0, windowHeight)),
    is_static = true
  )

  val init_b2 = BodyState(
    nextId,
    1,
    DVec.zero,
    DVec(0, 0),
    DVec(windowWidth / 2, windowHeight / 2),
    0.0,
    0.0,
    30.0,
    BoxShape(20, 20),
    is_static = false
  )

  val init_b3 = BodyState(
    nextId,
    1,
    DVec.zero,
    DVec(0, 0),
    DVec(windowWidth / 2, windowHeight / 2 + 100),
    0.0,
    0.0,
    30.0,
    BoxShape(20, 20),
    is_static = false
  )

  var b2 = init_b2
  var b3 = init_b3

  val evolution = SystemEvolutionStream
    .from(
      dt = 1.0 / 63,
      maxMultiplier = 1000000,
      base_dt = 1.0 / 63,
      force = (tacts, bs, other) => DVec(0, -9.81 * bs.mass),
      changeFunction = (tacts, bodies) =>
        (
          tacts,
          bodies.map(b =>
            b.index match {
              case init_b2.index => b2
              case init_b3.index => b3
              case _ => b
            }
          )
        )
    )(0, List(w1, w2, w3, w4, init_b2, init_b3))
    .iterator

  key(KEY_SPACE, onKeyDown = b2 = b2.copy(vel = b2.vel + DVec(0, 20), ang_vel = 10))

  action {
    val x = evolution.next()
    x._2
      .find(_.index == b2.index)
      .foreach(y => {
        b2 = y
      })
    x._2
      .find(_.index == b3.index)
      .foreach(y => {
        b3 = y
      })
  }

  render {
    openglLocalTransform {
      openglMove(b2.coord)
      openglRotateDeg(b2.ang)
      drawRectCentered(DVec.zero, b2.shape.asInstanceOf[BoxShape].width, b2.shape.asInstanceOf[BoxShape].height, WHITE)
    }

    openglLocalTransform {
      openglMove(b3.coord)
      openglRotateDeg(b3.ang)
      drawRectCentered(DVec.zero, b3.shape.asInstanceOf[BoxShape].width, b3.shape.asInstanceOf[BoxShape].height, WHITE)
    }
  }
}
