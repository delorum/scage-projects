package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceElement}

class AngularVelocityInfo extends InterfaceElement {
  private var str = ""
  private val strings:Array[String] = Array("")
  override protected def _update(): Unit = {
    str = f"Угловая скорость: ${OrbitalKiller.ship.angularVelocity}%.2f град/сек"
    strings(0) = str
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "AV"
}
