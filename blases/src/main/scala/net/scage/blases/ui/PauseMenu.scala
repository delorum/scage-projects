package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.blases.Blases._
import net.scage.Scage
import net.scage.ScageLib._
import net.scage.blases.Blases
import net.scage.blases.Relatives._

object PauseMenu {
  private val continue_level_button = new Button(xml("button.continue"), Vec(512, 384) + Vec(-60, 40), 100, Blases, {
    hide()
  }, RED, false)
  private val next_level_button = new Button(xml("button.nextlevel"), Vec(512, 384) + Vec(-60, 40), 100, Blases, {
    if(current_level < levels.length-1) {
      current_level += 1
      restart()
      hide()
    }
  }, RED, false)
  private val replay_level_button = new Button(xml("button.replaylevel"), Vec(512, 384) + Vec(-60, 0), 100, Blases, {
    if(score_updated) score -= score_for_level
    blases_shot -= blases_shot_on_level
    restart()
    hide()
  }, RED, false)
  private val main_menu_button = new Button(xml("button.tomainmenu"), Vec(512, 384) + Vec(-60, -40), 100, Blases, {
    stop()
  }, RED, false)
  private val exit_button = new Button(xml("button.exit"), Vec(512, 384) + Vec(-60, -120), 100, Blases, {
    Scage.stopApp()
  }, RED, false)
  private val play_game_again_button = new Button(xml("button.playallagain"), Vec(512, 384) + Vec(-60, 40), 100, Blases, {
    score = 0
    current_level = 0
    restart()
    hide()
  }, RED, false)
  private val help_button = new Button(xml("button.help"), Vec(512, 384) + Vec(-60, -80), 100, Blases, {
    HelpMenu.run()
    backgroundColor = BLACK
  }, RED, false)

  val WIN_LEVEL = 0
  val LOSE_LEVEL = 1
  val BEAT_GAME = 2
  val PRESS_ESC = 3

  private var _status = -1
  def status = _status

  interface {
    _status match {
      case WIN_LEVEL =>
        print(xml("pause.winlevel", score_for_level, score), rVec(512-60, 384+140), RED)
      case LOSE_LEVEL =>
        print(xml("pause.lose", score), rVec(512-60, 384+80), RED)
      case BEAT_GAME =>
        print(xml("pause.wingame", score), rVec(512-60, 384+100), RED)
      case PRESS_ESC =>
        print(xml("pause.pause"), rVec(512-60, 384+100), RED)
      case _ =>
    }
  }

  def showWinLevelMenu() {
    pause()
    _status = WIN_LEVEL
    next_level_button.visible   = true  // 40
    replay_level_button.visible = true  // 0
    main_menu_button.visible    = true  // -40
    help_button.visible         = true
    exit_button.visible         = true  // -80
  }
  def showLoseLevelMenu() {
    pause()
    _status = LOSE_LEVEL
    replay_level_button.visible = true  // 40
    main_menu_button.visible    = true  // 0
    help_button.visible         = true
    exit_button.visible         = true  // -40
  }
  def showBeatGameMenu() {
    pause()
    _status = BEAT_GAME
    play_game_again_button.visible = true   // 40
    replay_level_button.visible    = true   // 0
    main_menu_button.visible       = true   // -40
    help_button.visible            = true
    exit_button.visible            = true   // -80
  }
  def showEscMenu() {
    pause()
    _status = PRESS_ESC
    continue_level_button.visible = true    // 40
    replay_level_button.visible   = true    // 0
    main_menu_button.visible      = true    // -40
    help_button.visible           = true
    exit_button.visible           = true    // -80
  }
  def hide() {
    _status = -1
    continue_level_button.visible  = false
    next_level_button.visible      = false
    replay_level_button.visible    = false
    main_menu_button.visible       = false
    help_button.visible            = false
    exit_button.visible            = false
    play_game_again_button.visible = false
    pauseOff()
  }
}
