package com.github.dunnololda.scageprojects.orbitalkiller

abstract class InterfaceElement {
  protected def forceMinimizeIf:Boolean = false

  private var _minimized = false
  final def isMinimized = _minimized

  final def show(): Unit = {
    _minimized = false
  }
  final def hide(): Unit = {
    _minimized = true
  }

  protected def _update()

  final def update(): Unit = {
    if(!_minimized) {
      _update()
    }
  }

  protected def _data:Seq[String]

  final def data:Seq[String] = {
    if(!_minimized) _data else Seq.empty[String]
  }
}
