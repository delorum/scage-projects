package com.github.dunnololda.scageprojects.td

import com.github.dunnololda.scage.ScageLib._
import TowerDemka._

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) {
  private var lifetime = 100
  private val dir = direction.n
  private var coord = init_coord

  actionStaticPeriod(10) {
    if(lifetime > 0) {
      coord += dir
      lifetime -= 1
    } else deleteSelf()
  }

  render {
    if(lifetime > 0) {
      printCentered(message, coord, color)
    } else deleteSelf()
  }
}