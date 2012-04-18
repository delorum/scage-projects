package net.scage.blases.ui

import net.scage.handlers.controller2.MultiController
import net.scage.Screen
import net.scage.support.Vec
import net.scage.blases.levels._
import net.scage.ScageLib._
import net.scage.blases.Relatives._
import net.scage.blases.LevelSelector._
import net.scage.blases.{IntersectablePolygon, Blases, BonusLevel, Level}

/**
 * LevelButton is not like a button with onBtnDown logic and stuff, because all LevelButton presses produce exactly
 * the same - load the particular level. Although loading algorithm requires that Screen to be ran from MainMenu, so
 * LevelButton cannot be created in any Screen, only there...
 *
 * @param level - the level to load
 * @param level_num - the level's number in  the container
 * @param coord - LevelButton place on the screen
 */
case class LevelButton(level:Level,
                       level_num:Int,
                       coord:Vec) extends IntersectablePolygon {
  def intersectableVertices = List(rVec(coord) + Vec(-20, 20),
    rVec(coord) + Vec(20, 20),
    rVec(coord) + Vec(20, -20),
    rVec(coord) + Vec(-20, -20))
}

object LevelsMenu extends Screen("Blases Levels") with MultiController {
  val level1_button      = new LevelButton(Level1,      1, Vec(512, 384) + Vec(-(30+40)*3, 120))
  val level2_button      = new LevelButton(Level2,      2, Vec(512, 384) + Vec(-(30+40)*2, 120))
  val level3_button      = new LevelButton(Level3,      3, Vec(512, 384) + Vec(-(30+40)*1, 120))
  val level4_button      = new LevelButton(Level4,      4, Vec(512, 384) + Vec(-(30+40)*0, 120))
  val level5_button      = new LevelButton(Level5,      5, Vec(512, 384) + Vec( (30+40)*1, 120))
  val level6_button      = new LevelButton(Level6,      6, Vec(512, 384) + Vec( (30+40)*2, 120))
  val bonuslevel1_button = new LevelButton(BonusLevel1, 7, Vec(512, 384) + Vec( (40+40)*3, 120))
  val level7_button      = new LevelButton(Level7,      8, Vec(512, 384) + Vec(-(30+40)*3, 40))
  val level8_button      = new LevelButton(Level8,      9, Vec(512, 384) + Vec(-(30+40)*2, 40))

  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-40, -40), 100, LevelsMenu, stop())

  private val all_buttons = List(level1_button,
                                 level2_button,
                                 level3_button,
                                 level4_button,
                                 level5_button,
                                 level6_button,
                                 bonuslevel1_button, 
                                 level7_button, 
                                 level8_button)
  
  interface {
    all_buttons.foreach {
      case LevelButton(level, level_num, coord) =>
        if(level.is_passed) currentColor = BLACK
        else currentColor = GRAY
        drawRectCentered(rVec(coord), 40, 40)
        print(level_num, rVec(coord) - Vec(5, 5))
    }

    all_buttons.find(_.containsCoord(mouseCoord)) match {
      case Some(LevelButton(bonus_level:BonusLevel, _, _)) =>
        if(bonus_level.is_passed) {
          print(xml("level.stats", bonus_level.score_for_level, bonus_level.blases_shot_on_level), 10, 10, BLACK)
        } else {
          print(bonus_level.bonusConditionDescription, 10, 10, BLACK)
        }
      case Some(LevelButton(level:Level, _, _)) if(level.is_passed) =>
        print(xml("level.stats", level.score_for_level, level.blases_shot_on_level), 10, 10, BLACK)
      case _ =>
    }
  }

  leftMouseNoPause(onBtnDown = m =>
    all_buttons.find {
      case button @ LevelButton(level, level_num, _) => level.is_passed && button.containsCoord(m)
    } match {
      case Some(LevelButton(level, level_num, _)) =>
        currentLevelNum = level_num
        stop()
        MainMenu.runNextScreen(Blases)
      case _ =>
    }
  )
}
