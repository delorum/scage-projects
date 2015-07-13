package com.github.dunnololda.scageprojects.orbitalkiller

abstract class InterfaceElement {
  def shortDescr:String

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

  override def toString = shortDescr
}
