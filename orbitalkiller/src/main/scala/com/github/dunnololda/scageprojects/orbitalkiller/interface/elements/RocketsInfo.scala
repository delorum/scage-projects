package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement

class RocketsInfo extends InterfaceElement {
  private val strings:Array[String] = Array("\u21e7")

  override def shortDescr: String = "R"

  override def data: Seq[String] = strings

  override protected def _update(): Unit = {}
}
