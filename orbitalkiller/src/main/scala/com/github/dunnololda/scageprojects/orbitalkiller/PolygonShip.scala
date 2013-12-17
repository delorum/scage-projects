package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

class PolygonShip(
  val index:String,
  val vertices:List[Vec],
  val engines_mapping:Map[Int, Engine],
  init_coord:Vec,
  init_velocity:Vec = Vec.zero,
  init_rotation:Float = 0f) {

}
