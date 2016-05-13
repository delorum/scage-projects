package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement

class OtherShipInfoMinimized(num: Int) extends InterfaceElement {
  override val shortDescr: String = s"OS($num)"

  override def data: Seq[String] = Seq()

  override protected def _update(): Unit = {}

  override val color = ScageColor.MAGENTA
}
