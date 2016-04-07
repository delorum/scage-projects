package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceHolder, InterfaceElement}

class OtherShipInfoMinimized(num:Int) extends InterfaceElement {
  override def shortDescr: String = s"OS($num)"
  override def data: Seq[String] = Seq()
  override protected def _update(): Unit = {}
}
