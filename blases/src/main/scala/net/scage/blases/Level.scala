package net.scage.blases

import levelparts.FlyingWord
import net.scage.support.Vec
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Relatives._

trait Level {
  def load() {
    constructLevel()
    drawStartFinish()
  }

  def constructLevel()

  def startCoord: Vec
  def finishCoords: List[Vec]

  private def drawStartFinish() {
    val render_id = render {
      drawCircle(startCoord, rInt(20), rColor(RED))
      print(xml("level.start"), (startCoord - rVec(20, 40)), rColor(RED))

      finishCoords.foreach(finish_coord => {
        drawCircle(finish_coord, rInt(30), rColor(GREEN))
        print(xml("level.finish"), (finish_coord - rVec(25, 50)), rColor(GREEN))
      })
    }

    clear {
      delOperations(render_id, currentOperation)
    }
  }
}
