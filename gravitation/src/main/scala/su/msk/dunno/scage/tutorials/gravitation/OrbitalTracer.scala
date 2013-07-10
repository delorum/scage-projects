package su.msk.dunno.scage.tutorials.gravitation

import com.github.dunnololda.scage.ScageLib._
import Orbita._

object OrbitalTracer extends CoordTracer[MaterialPoint] {
  private var _time = 0L
  def time = _time

  action {
    val new_body_coords = traces_list.foldLeft(List[(MaterialPoint, Vec)]())((bodies_coords, body) => {
      body.updateVelocity(_time, traces_list)
      val new_body_coord = outsideCoord(body.location + body.velocity*dt)
      (body, new_body_coord) :: bodies_coords
    })
    new_body_coords.foreach(new_body_coord => updateLocation(new_body_coord._1, new_body_coord._2))
    _time += 1
  }
}