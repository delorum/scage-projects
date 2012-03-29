package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.support.physics.objects.StaticPolygon
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Blase

class Star(coord: Vec, num_beams: Int = 5, radius1: Int = 60, radius2: Int = 30) extends StaticPolygon({
  val radius = Array(radius1, radius2)
  (for {
    i <- 0 until num_beams * 2
    angle = ((180f / num_beams) * i * math.Pi / 180f)
  } yield coord + Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toArray
}) {
  physics.addPhysical(this)

  private val action_id = action {
    touchingBodies.foreach {
      body => {
        val user_data = body.getUserData
        if (user_data != null && user_data.isInstanceOf[Blase]) {
          val blase = user_data.asInstanceOf[Blase]
          score_for_level += 300
          new FlyingWord(300, YELLOW, blase.location, blase.velocity)
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
    deleteSelf()
  }
}
