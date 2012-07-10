package su.msk.dunno.scage.uke

import net.scage.support.physics.ScagePhysics
import net.scage.support.Vec
import net.scage.support.physics.objects.{StaticPolygon, DynaBox}
import net.scage.ScageScreenApp
import net.scage.ScageLib._

object Test extends ScageScreenApp("test"){
  val physics = new ScagePhysics

  val arr = List(Vec(window_width/2-250, 200), Vec(window_width/2+250, 200), Vec(window_width/2+250, 50), Vec(window_width/2-250, 50)).reverse
  val static_box = new StaticPolygon(arr:_*)
  static_box.body.setRotation(0.1f)
  val dyna_box = new DynaBox(Vec(window_width/2, 500), 50, 50)
  //val dyna_box = new DynaBall(Vec(window_width/2, 200), 50)

  physics.addPhysical(static_box)
  physics.addPhysical(dyna_box)

  backgroundColor = WHITE
  render {
    drawPolygon(static_box.points, BLACK)
    drawPolygon(dyna_box.points, BLACK)
    //drawCircle(dyna_box.coord, 50,  BLACK)
  }

  action {
    physics.step()
  }
}