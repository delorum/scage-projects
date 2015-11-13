package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceElement}
import com.github.dunnololda.scageprojects.orbitalkiller._

class TimeInfo extends InterfaceElement {
  private val strings = Array("", "")
  override protected def _update(): Unit = {
    strings(0) = s"Время: ${timeStr((OrbitalKiller.tacts*OrbitalKiller.base_dt*1000).toLong)}${if(OrbitalKiller._stop_after_number_of_tacts > 0) s" Остановка через ${timeStr((OrbitalKiller._stop_after_number_of_tacts*OrbitalKiller.base_dt*1000).toLong)}" else ""}"
    strings(1) = f"Ускорение времени: x${(OrbitalKiller.timeMultiplier*OrbitalKiller.k).toInt}/${OrbitalKiller.maxTimeMultiplier} (${1f*OrbitalKiller.timeMultiplier/63*OrbitalKiller.ticks}%.2f)"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "T"
}
