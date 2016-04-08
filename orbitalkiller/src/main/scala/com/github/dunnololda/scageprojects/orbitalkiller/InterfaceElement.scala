package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scage.support.messages.ScageMessage

abstract class InterfaceElement {
  def shortDescr:String // подразумевается, что в потомках это будет val - неизменяемое значение
  lazy val shortDescrLen: Int = ScageMessage.messageBounds(shortDescr).ix

  private var _minimized_by_user = false
  final def isMinimizedByUser = _minimized_by_user

  private var _minimized_by_constraint = false
  final def isMinimizedByConstraint = _minimized_by_constraint
  
  final def isMinimized = _minimized_by_user || _minimized_by_constraint

  final def showByUser(): Unit = {
    _minimized_by_user = false
  }

  final def showByConstraint(): Unit = {
    _minimized_by_constraint = false
  }

  final def hideByUser(): Unit = {
    _minimized_by_user = true
  }

  final def hideByConstraint(): Unit = {
    _minimized_by_constraint = true
  }

  private var _update_needed = true
  def markUpdateNeeded(): Unit = {
    _update_needed = true
  }

  protected def _update()

  final def updateIfNotMinimized(): Unit = {
    if(!isMinimized) {
      _update()
    }
  }

  def data:Seq[String]
  def color:ScageColor = ScageColor.YELLOW

  override def toString = shortDescr
}
