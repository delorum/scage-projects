package net.scage.blases

import net.scage.support.Vec
import net.scage.support.physics.objects.StaticPolygon
import net.scage.blases.Blases._
import net.scage.ScageLib._

class BurstPolygon(vertices: Vec*) extends StaticPolygon(vertices: _*) {
  physics.addPhysical(this)

  private val action_id = action {
    touchingBodies.foreach {
      body => {
        val user_data = body.getUserData
        if (user_data != null && user_data.isInstanceOf[Blase]) {
          user_data.asInstanceOf[Blase].burst()
          score_for_level -= 100
        }
      }
    }
  }

  private val render_id = render {
    drawPolygon(points, RED)
  }

  def remove() {
    physics.removePhysicals(this)
    delOperations(action_id, render_id)
  }
}
