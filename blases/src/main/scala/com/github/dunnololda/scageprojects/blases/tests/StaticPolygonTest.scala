package com.github.dunnololda.scageprojects.blases.tests

import com.github.dunnololda.scage.ScageLib._

object StaticPolygonTest extends ScageScreenApp("Static Polygon Test", 640, 480) {
  val physics = new ScagePhysics()
  action {
    physics.step()
  }

  val pew = physics.addPhysical(new StaticPolygon(Vec(100, 300), Vec(150, 250), Vec(300, 300), Vec(300, 450), Vec(200, 400)) {
    render {
      drawPolygon(points, RED)
      drawFilledCircle(coord, 3, YELLOW)
    }
  })

  key(KEY_LEFT,  onKeyDown = pew.coord += Vec(10,0))
  key(KEY_RIGHT, onKeyDown = pew.coord += Vec(-10,0))
  key(KEY_UP,    onKeyDown = pew.coord += Vec(0,10))
  key(KEY_DOWN,  onKeyDown = pew.coord += Vec(0,-10))

  private var start_coord = Vec.zero
  private var is_mouse_drag = false
  leftMouse(onBtnDown = {m => start_coord = m; is_mouse_drag = true}, onBtnUp = {m =>
    val init_velocity = (m - start_coord).n*10
    physics.addPhysical(new DynaBall(start_coord, 10) {
      velocity = init_velocity
      render {
        drawCircle(coord, 10, YELLOW)
      }
    })
    is_mouse_drag = false
  })

  private var drag_coord = Vec.zero
  leftMouseDrag(onDrag = {m => drag_coord = m})

  render {
    if(is_mouse_drag) drawLine(start_coord, drag_coord, YELLOW)
  }
}
