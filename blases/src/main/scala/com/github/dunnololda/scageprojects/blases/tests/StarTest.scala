package com.github.dunnololda.scageprojects.blases.tests

import com.github.dunnololda.scage.ScageLib._

class Star(coord:Vec, num_beams:Int = 5, radius1:Int = 60, radius2:Int = 30) extends StaticPolygon({
  val radius = Array(radius1, radius2)
  (for {
    i <- 0 until num_beams*2
    angle = (180f / num_beams) * i * math.Pi / 180f
  } yield coord + Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toArray
}) {

}

object StarTest extends ScageScreenApp("Star Test", 640, 480){
  val star = new Star(Vec.zero, num_beams = 5)

  private var ang = 0f
  actionStaticPeriod(200) {
    ang += 5f
  }

  render {
    openglMove(windowCenter)
    openglRotate(ang)
    drawPolygon(star.points, WHITE)
  }
}
