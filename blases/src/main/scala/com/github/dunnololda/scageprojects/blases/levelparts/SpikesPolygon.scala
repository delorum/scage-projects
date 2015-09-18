package com.github.dunnololda.scageprojects.blases.levelparts

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.Blase
import com.github.dunnololda.scageprojects.blases.Blases._
import com.github.dunnololda.scageprojects.blases.Relatives._

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
    drawPolygon(points, rColor(RED))
  }

  clear {
    physics.removePhysicals(this)
    delOperations(action_id, render_id, currentOperation)
  }
}
