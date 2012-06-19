package su.msk.dunno.scage.tutorials.life

import net.scage.support.tracer3.ScageTracer
import io.Source
import net.scage.support.Vec
import net.scage.{ScageScreenApp, ScageApplet}
import org.newdawn.slick.util.ResourceLoader
import net.scage.ScageLib._
import net.scage.support.messages.ScageMessage

case class NextGenCell(point:Vec, status:Int, id:Int = -1)

class LifeApplet extends ScageApplet {
  val screen = Life
}

/*class LifeApplet extends ScageApplet {*/
  object Life extends ScageScreenApp("Life") {
    val tracer = ScageTracer(solid_edges = true)
    import tracer._
    def loadLevel(level_name:String) {
      if(!tracesList.isEmpty) removeAllTraces()
      val level_rows = Source.fromInputStream(ResourceLoader.getResourceAsStream(level_name), "utf-8").getLines()
      for {
        (row, row_number) <- level_rows.zipWithIndex
        (char,  column_number) <- row.zipWithIndex
        if char == '0'
      } addTrace(Vec(column_number, N_y-1-row_number))
    }
    loadLevel(property("level", "resources/levels/glidergun.life"))

    backgroundColor = DARK_GRAY

    render {
      tracesList.foreach(trace => drawFilledRectCentered(pointCenter(trace.location), h_x, h_y, YELLOW))
      drawTraceGrid(tracer, GRAY)
    }

    interface {
      if(onPause) ScageMessage.print("PAUSE", 20, windowHeight-20, YELLOW)
    }

    leftMouse(onBtnDown = mouse_coord => if(tracesInPoint(point(mouse_coord)).length == 0) addTrace(point(mouse_coord)))
    rightMouse(onBtnDown = mouse_coord => removeTraces(tracesInPoint(point(mouse_coord)):_*))

    keyNoPause(KEY_SPACE, onKeyDown = switchPause())
    keyNoPause(KEY_C, onKeyDown = removeAllTraces())

    val BORN  = 0
    val DIES  = 1
    action(/*speed*/50) {
      val next_gen_points = (for {
        trace <- tracesList
        i <- -1 to 1
        j <- -1 to 1
        point = outsidePoint(trace.location + Vec(i, j))
        if isPointOnArea(point)
      } yield point).toSet

      val next_gen_changes = for {
        current_point <- next_gen_points
        (is_containing_trace, id) = {
          val traces_in_point = tracesInPoint(current_point)
          if(traces_in_point.length > 0) (true, traces_in_point.head.id)
          else (false, -1)
        }
        neighbours_length = tracesNearPoint(current_point, -1 to 1, cell => cell.location != current_point).length
      } yield {
        if(is_containing_trace) {
          if(neighbours_length > 3 || neighbours_length < 2) NextGenCell(current_point, DIES, id)
        } else if(neighbours_length == 3) NextGenCell(current_point, BORN)
      }

      next_gen_changes.foreach(_ match {
        case NextGenCell(_, DIES, id) => removeTracesById(id)
        case NextGenCell(point, BORN, _) =>
          if(tracesInPoint(point).length != 0) println("already have point!!!!!")
          addTrace(point)
        case _ =>
      })
    }

    clear {
      delAllOperations()
      removeAllTraces()
    }

    pause()
  }
/*
}*/
