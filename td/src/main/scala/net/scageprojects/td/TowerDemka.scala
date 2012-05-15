package net.scageprojects.td

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.tracer3.CoordTracer

object TowerDemka extends ScageScreenApp("Tower Demka", 800, 600) {
  val tracer = CoordTracer(
    field_from_x = 10,
    field_to_x = 790,
    field_from_y = 110,
    field_to_y = 590,
    init_h_x = 20,
    init_h_y = 20
  )
}
