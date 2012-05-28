package net.scageprojects.td

import net.scage.ScageLib._
import net.scageprojects.td.TowerDemka._
import net.scage.support.Vec
import net.scage.support.tracer3.DefaultTrace

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

  render {
    drawRectCentered(tracer.pointCenter(Vec(tracer.N_x-1, tracer.N_y/2)), tracer.h_x, tracer.h_y*tracer.N_y, GREEN)
  }
}
