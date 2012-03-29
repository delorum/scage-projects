package net.scage.blases

import net.scage.support.physics.objects.DynaBall
import net.scage.support.tracer3.Trace
import net.scage.support.{State, Vec}
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Relatives._

class Blase(init_coord: Vec, direction:Vec) extends DynaBall(init_coord, radius = rInt(20)) with Trace {
  velocity = direction.n*rInt(90)
  
  physics.addPhysical(this)
  tracer.addTrace(coord, this)
  body.setUserData(this)

  def state = State()

  def changeState(changer: Trace, s: State) {}

  private val action_id = action {
    tracer.updateLocation(this, coord)
    coord = this.location
  }

  private val render_id = render {
    val color = if (id == selected_blase.id) RED else WHITE
    drawCircle(coord, radius, color)
  }

  def burst() {
    delOperations(action_id, render_id)
    tracer.removeTraces(this)
    physics.removePhysicals(this)
    if(selected_blase == this) selected_blase = no_selection
  }
}
