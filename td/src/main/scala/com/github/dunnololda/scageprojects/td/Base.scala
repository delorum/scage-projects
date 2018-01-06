package com.github.dunnololda.scageprojects.td

import com.github.dunnololda.scage.ScageLib._
import TowerDemka._

object Base extends SelfHitPoints with BaseType {
  hp = property("base.hp", 50f)

  init {
    hp = property("base.hp", 50f)
  }

  private def base_hp = hp
  private def base_hp_=(new_base_hp:Float) {hp = new_base_hp}

  for(y <- 0 until tracer.N_y) tracer.addTrace(tracer.pointCenter(Vec(tracer.N_x-1, y)), new DefaultTrace with BaseType with HaveHitPoints with SelfRemovable with Damageable {
    def hp = base_hp
    def hp_=(new_hp:Float) {base_hp = new_hp}

    def remove() {restart()}
  })

  private val base_coord = tracer.pointCenter(Vec(tracer.N_x-1, tracer.N_y/2))
  private val base_height = tracer.h_y*tracer.N_y
  render {
    drawRectCentered(base_coord, tracer.h_x, base_height, GREEN)
    printCentered(hp, base_coord)
  }
}
