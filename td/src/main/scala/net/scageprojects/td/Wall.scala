package net.scageprojects.td

import net.scage.support.Vec
import net.scage.support.tracer3.DefaultTrace
import net.scage.ScageLib._
import net.scageprojects.td.TowerDemka._

object Wall {
  val wall_price = property("wall.price", 7)
  val wall_max_hp = property("wall.hp", 70)
}

import Wall._

class Wall(init_point:Vec) extends DefaultTrace with SelfHitPoints with WallType with SelfRemovable with Damageable with SelfInsertable {
  hp = wall_max_hp

  def init_coord = tracer.pointCenter(init_point)

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, BLUE)
    printCentered(hp.formatted("%.0f"), location)
  }

  def remove() {
    hp = 0
    delOperations(render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}
