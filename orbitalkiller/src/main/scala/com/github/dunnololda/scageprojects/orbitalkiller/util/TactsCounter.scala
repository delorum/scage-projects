package com.github.dunnololda.scageprojects.orbitalkiller.util

/**
  * TODO
  *
  * @author aborunov
  */
class TactsCounter(init_tacts: Int = 0) {
  private var _tacts: Int = init_tacts

  def tacts: Int = _tacts

  def done: Boolean = _tacts == 0
  def notDone: Boolean = _tacts != 0

  def inc(amount: Int = 1): Unit = {
    _tacts += amount
  }

  def dec(amount: Int = 1): Unit = {
    _tacts = scala.math.max(0, _tacts - amount)
  }

  def decIfNotDoneAndCheck(amount: Int = 1): Boolean = {
    if (notDone) dec(amount)
    done
  }

  def reset(): Unit = {
    _tacts = 0
  }
}
