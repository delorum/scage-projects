package com.github.dunnololda.scageprojects.scarcanoid.levels

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.scarcanoid.Scaranoid._
import com.github.dunnololda.scageprojects.scarcanoid.TargetBox

trait LevelMap {
  val rows = 4
  val columns = 17
  def level:List[Int]

  def load = {
    (for {
      i <- 0 until rows
      j <- 0 until columns
      if level(i*columns + j) == 1
      box = physics.addPhysical(new TargetBox(Vec(55 + j*45, windowHeight-40-45*i)))
    } yield box).toList
  }
}