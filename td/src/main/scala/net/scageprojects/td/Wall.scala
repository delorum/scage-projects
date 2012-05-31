package net.scageprojects.td

import net.scage.ScageLib._
import net.scageprojects.td.TowerDemka._
import net.scage.support.tracer3.{Trace, DefaultTrace}
import net.scage.support.{State, Vec}

object Wall {
  val wall_price = property("wall.price", 7)
  val wall_max_hp = property("wall.hp", 70)
  val wall_repair_price = property("wall.repair.price", 10)
}

import Wall._

class Wall(init_point:Vec) extends DefaultTrace with SelfHitPoints with WallType with SelfRemovable with Damageable with SelfInsertable {
  hp = wall_max_hp

  def init_coord = tracer.pointCenter(init_point)

  override def changeState(changer:Trace, s:State) {
    super.changeState(changer, s)
    s.neededKeys {
      case ("mouse_clicked", mouse_coord:Vec) =>
        val y_offset = location.y - mouse_coord.y
        if(y_offset >= 24 && y_offset < 37) {  // repair
          val repair_price = wall_repair_price + (wall_max_hp - hp.toInt)/2
          if(resource >= repair_price) {
            hp = wall_max_hp
            callEvent("Building Repaired", repair_price)
          }
        }
    }
  }

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, BLUE)
    val wall_info = "HP: "+hp.formatted("%.0f")+"\n\n\n"+ {
      if(hp < wall_max_hp) {
        val repair_price = wall_repair_price + (wall_max_hp-hp.toInt)/2
        if(resource >= repair_price) "[rRepair ("+repair_price+")]" else "Repair ("+repair_price+")"
      } else ""
    }/*"[rRepair]"*/
    small_font.print(wall_info, location + Vec(-35, 19))
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
