package com.github.dunnololda.scageprojects.blases.tests

class Button(message: String, message_coord: Vec, width: Int, screen: Screen, onBtnPressed: => Any) {
  val vertices = List(Vec(message_coord) + Vec(-5, 20),
    Vec(message_coord) + Vec(-5 + width, 20),
    Vec(message_coord) + Vec(-5 + width, -10),
    Vec(message_coord) + Vec(-5, -10))
  val vertices_zipped = if (vertices.length >= 2) {
    val vertices_shift = vertices.last :: vertices.init
    vertices_shift.zip(vertices)
  } else List[(Vec, Vec)]()
  def containsCoord(coord: Vec): Boolean = {
    def _areLinesIntersect(a: Vec, b: Vec, c: Vec, d: Vec): Boolean = {
      val common = (b.x - a.x) * (d.y - c.y) - (b.y - a.y) * (d.x - c.x)
      if (common == 0) false
      else {
        val rH = (a.y - c.y) * (d.x - c.x) - (a.x - c.x) * (d.y - c.y)
        val sH = (a.y - c.y) * (b.x - a.x) - (a.x - c.x) * (b.y - a.y)

        val r = rH / common
        val s = sH / common

        if (r >= 0 && r <= 1 && s >= 0 && s <= 1) true
        else false
      }
    }
    if (vertices.length < 2) false
    else {
      val a = coord
      val b = Vec(Integer.MAX_VALUE, coord.y)
      val intersections = vertices_zipped.foldLeft(0) {
        case (result, (c, d)) => if (_areLinesIntersect(a, b, c, d)) result + 1 else result
      }
      intersections % 2 != 0
    }
  }
  
  var visible = true

  screen.render {
    if(visible) {
      drawPolygon(vertices, BLACK)
      print(message, Vec(message_coord), BLACK)
    }
  }
  
  screen.leftMouse(onBtnDown = {m => if(visible && containsCoord(m)) onBtnPressed})
}

object MultiControllerTest extends ScreenApp("MultiController Test", width=640, height = 480) with MultiController {
  val pressme_button = new Button("Press Me", Vec(320, 240) + Vec(-40, 40), 100, MultiControllerTest, {
    println("MultiControllerTest: Pressed")
    MultiControllerTestScreen2.run()
  })

  backgroundColor = WHITE
}

object MultiControllerTestScreen2 extends Screen("MultiController Test Screen2") with MultiController {
  preinit {
    windowTitle = "MultiController Test Screen2"
  }

  val exit_button = new Button("Exit", Vec(320, 240) + Vec(-40, 40), 100, MultiControllerTestScreen2, {
    println("MultiControllerTestScreen2: Pressed")
    stop()
  })
}