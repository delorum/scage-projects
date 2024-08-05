package com.github.dunnololda.scageprojects.orbitalkiller_cake.util

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{
  Body => Phys2dBody,
  BodyList => Phys2dBodyList,
  Vector2f
}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.BodyState

object Phys2dUtils {

  implicit class DVec2DoublePhys2dVector(v: DVec) {
    def toPhys2dVecD = new Vector2f(v.x, v.y)
  }

  implicit class Phys2dBody2BodyState(pb: Phys2dBody) {

    def toBodyState: Option[BodyState] = {
      pb.getUserData match {
        case (index: Int, shape: Shape) =>
          Some(
            BodyState(
              index = index,
              mass = pb.getMass,
              acc = DVec.dzero,
              vel = DVec(pb.getVelocity.getX, pb.getVelocity.getY),
              coord = DVec(pb.getPosition.getX, pb.getPosition.getY),
              ang_acc = 0.0,
              ang_vel = pb.getAngularVelocity.toDeg,
              ang = pb.getRotation.toDeg,
              shape = shape,
              is_static = pb.isStatic
            )
          )
        case _ => None
      }
    }
  }

  implicit class Phys2dBodyList2List(pbl: Phys2dBodyList) {

    def toList: List[(Phys2dBody, BodyState)] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield (pb, bs)).toList
    }

    def toBodyStateList: List[BodyState] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield bs).toList
    }
  }
}
