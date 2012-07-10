package su.msk.dunno.scar.levels

import su.msk.dunno.scar.TargetBox
import su.msk.dunno.scar.Scaranoid._
import net.scage.support.Vec

trait LevelMap {
  val rows = 4
  val columns = 17
  def level:List[Int]

  def load = {
    (for {
      i <- 0 until rows
      j <- 0 until columns
      if level(i*columns + j) == 1
      box = physics.addPhysical(new TargetBox(Vec(55 + j*45, window_height-40-45*i)))
    } yield box).toList
  }
}