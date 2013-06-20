package su.msk.dunno.scage.uke

import com.github.dunnololda.scage.ScageLib._

object Test extends ScageScreenApp("test", 800, 600){
  val physics = ScagePhysics()

  val arr = List(Vec(windowWidth/2-250, 200), Vec(windowWidth/2+250, 200), Vec(windowWidth/2+250, 50), Vec(windowWidth/2-250, 50)).reverse
  val static_box = new StaticPolygon(arr:_*)
  static_box.body.setRotation(0.1f)
  val dyna_box = new DynaBox(init_coord = Vec(windowWidth/2, 500), box_width = 50, box_height = 50, 1, true)
  //val dyna_box = new DynaBall(Vec(windowWidth/2, 200), 50)

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