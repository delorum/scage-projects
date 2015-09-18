package com.github.dunnololda.scageprojects.blases

import com.github.dunnololda.scage.ScageLib._
import Blases._
import Relatives._

trait Level {
  var is_entered = false         // TODO: maybe make them private and create pass() method which changes is_passed value and updates scores
  var score_for_level = 0
  var blases_shot_on_level = Integer.MAX_VALUE

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
      print(xml("level.start"), startCoord - rVec(20, 40), rColor(RED))

      finishCoords.foreach(finish_coord => {
        drawCircle(finish_coord, rInt(30), rColor(GREEN))
        print(xml("level.finish"), finish_coord - rVec(25, 50), rColor(GREEN))
      })
    }

    clear {
      delOperations(render_id, currentOperation)
    }
  }
}

trait BonusLevel extends Level {
  def bonusCondition:Boolean
  def bonusConditionDescription:String
}
