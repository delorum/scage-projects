package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib.{DynaBox, ScagePhysics, StaticLine, Vec, stopApp, _}

object PurePhys2dTest extends ScageScreenApp("Pure Phys2d Test", 640, 480) {
  val physics = new ScagePhysics(1, Vec(0, -9.8))
  physics.world.enableRestingBodyDetection(0.01f, 0.1f, 0.1f)

  val w = windowWidth/2
  val h = windowHeight/2

  physics.addPhysical(new StaticLine(Vec(w-100, h-100),  Vec(w-100, h+100)))
  physics.addPhysical(new StaticLine(Vec(w-100, h+100),  Vec(w+100, h+100)))
  physics.addPhysical(new StaticLine(Vec(w+100, h+100),  Vec(w+100, h-100)))
  physics.addPhysical(new StaticLine(Vec(w+100, h-100),  Vec(w-100, h-100)))

  val b = new DynaBox(Vec(w-60, h), 30f, 20f, 1f)
  b.body.setRotation(10)
  physics.addPhysical(b)

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL)) stopApp())

  action {
    physics.step()
  }

  render {
    drawLine(Vec(w-100, h-100),  Vec(w-100, h+100), WHITE)
    drawLine(Vec(w-100, h+100),  Vec(w+100, h+100), WHITE)
    drawLine(Vec(w+100, h+100),  Vec(w+100, h-100), WHITE)
    drawLine(Vec(w+100, h-100),  Vec(w-100, h-100), WHITE)

    drawSlidingLines(b.points.toList ::: List(b.points.head), WHITE)
  }
}
