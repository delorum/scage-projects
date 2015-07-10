package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceElement}

class FailedToLoadInfo extends InterfaceElement {
  override def minimizable:Boolean = false

  private val rows1 = List("Не удалось загрузить игру")
  private var strings:List[String] = Nil
  override def _update() {
    if(OrbitalKiller.showGameLoadedMessage) strings = rows1
    else strings = Nil
  }
  override def _data: Seq[String] = strings
}

