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
  def finishCoord: Vec

  private def drawStartFinish() {
    val render_id = render {
      drawCircle(startCoord, rInt(20), RED)
      print("Start", (startCoord - rVec(20, 40)), RED)

      drawCircle(finishCoord, rInt(30), GREEN)
      print("Finish", (finishCoord - rVec(25, 50)), GREEN)
    }

    clear {
      delRenders(render_id)
      deleteSelf()
    }
  }

  def isWin: Boolean = {
    val winner_blases = tracer.tracesNearCoord(finishCoord, -1 to 1, condition = {
      blase => blase.location.dist(finishCoord) < 20
    })
    val is_win = !winner_blases.isEmpty
    if(is_win) new FlyingWord(score_for_level, YELLOW, winner_blases.head.location, winner_blases.head.velocity)
    is_win
  }
}
