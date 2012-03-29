package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.support.physics.objects.StaticPolygon
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Blase

class SpikesPolygon(vertices: Vec*) extends StaticPolygon(vertices: _*) {
  physics.addPhysical(this)

  private val action_id = action {
    touchingBodies.foreach {
      body => {
        val user_data = body.getUserData
        if (user_data != null && user_data.isInstanceOf[Blase]) {
          val blase = user_data.asInstanceOf[Blase]
          blase.burst()
          score_for_level -= 100
          new FlyingWord(-100, YELLOW, blase.location, blase.velocity)
        }
      }
    }
  }

  private val render_id = render {
    drawPolygon(points, RED)
  }

  clear {
    physics.removePhysicals(this)
    delOperations(action_id, render_id)
  }
}
