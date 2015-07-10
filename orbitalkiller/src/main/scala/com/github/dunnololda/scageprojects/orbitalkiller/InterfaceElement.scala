package com.github.dunnololda.scageprojects.orbitalkiller

abstract class InterfaceElement {
  def minimizable:Boolean = true

  private var _minimized = false
  final def isMinimized = _minimized

  final def show(): Unit = {
    _minimized = false
  }
  final def hide(): Unit = {
    if(minimizable) _minimized = true
  }

  protected def _update()

  final def update(): Unit = {
    if(!_minimized || !minimizable) {
      _update()
    }
  }

  protected def _data:Seq[String]

  final def data:Seq[String] = {
    if(!_minimized || !minimizable) _data else Seq.empty[String]
  }
}
