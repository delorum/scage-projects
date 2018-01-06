package com.github.dunnololda.scageprojects.blases.tests

import net.phys2d.raw.FixedJoint
import com.github.dunnololda.scage.ScageLib._

object JointsTest extends ScageScreenApp("Joints Test", 640, 480) {
  val physics = ScagePhysics()
  physics.addPhysicals(ball1, box1)

  val ball1 = new DynaBall(Vec(200, 100), 10)

  val box1 = new StaticBox(Vec(200, 200), 20, 20) {
    private val step = (Vec(400, 400) - Vec(200, 200)).n
    private var dir = 1
    val speed = 1f
    action {
      coord += step*speed*dir
      dir match {
        case 1 =>
          if((coord dist Vec(400, 400)) < 5) {
            dir = -1
            physics.world.remove(joint1)
            physics.world.add(joint1)
          }
        case -1 =>
          if((coord dist Vec(200, 200)) < 5) {
            dir = 1
            physics.world.remove(joint1)
            physics.world.add(joint1)
          }
        case _ =>
      }
    }
  }

  val joint1:FixedJoint = new FixedJoint(ball1.body, box1.body)
  //val joint2 = new FixedAngleJoint(ball1.body, box1.body, ball1.coord.toPhys2dVec, box1.coord.toPhys2dVec, (ball1.coord - box1.coord).rad(Vec(0,1)))

  //physics.world.add(joint2)

  physics.world.add(joint1)

  action {
    physics.step()
  }

  backgroundColor = WHITE
  render {
    drawCircle(ball1.coord, 10, BLACK)
    drawPolygon(box1.points, BLACK)
    drawLine(ball1.coord, box1.coord, BLACK)
    print(box1.coord dist ball1.coord, 20, 20, BLACK)
  }
}
