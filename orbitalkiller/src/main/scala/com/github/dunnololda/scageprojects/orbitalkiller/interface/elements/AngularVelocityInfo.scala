package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceElement}
import com.github.dunnololda.scageprojects.orbitalkiller._

class AngularVelocityInfo extends InterfaceElement {
  private var str = ""
  private val strings:Array[String] = Array("")
  override protected def _update(): Unit = {
    // это сила, приложенная непосредственно к пилоту, так что она не оч информативна, в частности, сила двигателей должна быть гораздо больше, чтобы парировать это
    //val centrifugial_force = if (OrbitalKiller.ship.angularVelocity == 0) 0.0 else OrbitalKiller.ship.pilot_mass * math.pow(OrbitalKiller.ship.angularVelocity.toRad, 2) * OrbitalKiller.ship.pilot_position.norma
    if(OrbitalKiller.ship.angularVelocity.abs >= 0.01) {
      str = f"Угловая скорость: ${OrbitalKiller.ship.angularVelocity}%.2f град/сек"
    } else {
      str = f"Угловая скорость: 0.00 град/сек"
    }
    strings(0) = str
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "AV"
}
