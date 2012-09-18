package su.msk.dunno.scage.tutorials.gravitation

import net.scage.support.tracer3.CoordTracer
import net.scage.support.{ScageColor, Vec}
import net.scage.ScageLib._
import Orbita._
import collection.mutable.ArrayBuffer

object TrajectoryTracer extends CoordTracer[MaterialPoint] {
  val iterations_per_moment = property("trajectory.iterations", 5)

  private var is_calculating_trajectories = false
  private val points = ArrayBuffer[Vec]()
  private val ship_points = ArrayBuffer[Vec]()
  def calculateTrajectories() {
    is_calculating_trajectories = true
    points.clear()
    ship_points.clear()
    removeAllTraces()
    OrbitalTracer.tracesList.foreach(body => {
      addTrace(body.location, body.copy)
    })
  }

    def stopCalculating() {
    is_calculating_trajectories = false
  }

  actionNoPause {
    if(is_calculating_trajectories) {
      for(i <- 1 to iterations_per_moment) {
        val new_body_coords = traces_list.foldLeft(List[(MaterialPoint, Vec)]())((bodies_coords, body) => {
          body.updateVelocity(traces_list)
          val new_body_coord = outsideCoord(body.location + body.velocity*dt)
          (body, new_body_coord) :: bodies_coords
        })
        new_body_coords.foreach {
          case (body, coord) => {
            //val color = if(body.id == Orbita.ship.id) CYAN else GREEN
            updateLocation(body, coord)
            if(body.id != Orbita.ship.id) points += body.location.copy
            else ship_points += body.location.copy
          }
        }
      }
    }
  }

  render {
    if(is_calculating_trajectories) {
      drawPoints(points, GREEN)
      drawPoints(ship_points, CYAN)
    }
  }
}