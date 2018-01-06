package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller
import com.github.dunnololda.scageprojects.orbitalkiller.interfaces.InterfaceElement

class TimeInfo extends InterfaceElement {
  private val strings_without_stop_moment = Array("")
  private val strings_with_stop_moment = Array("", "")
  private var strings = strings_without_stop_moment

  override protected def _update(): Unit = {
    val time_acceleration = f"x${(OrbitalKiller.timeMultiplier * OrbitalKiller.k).toInt} (${1f * OrbitalKiller.timeMultiplier / 63 * OrbitalKiller.ticks}%.2f)"
    if (OrbitalKiller._stop_after_number_of_tacts > 0 || OrbitalKiller._set_stop_time) {
      strings_with_stop_moment(0) = s"Время: $time_acceleration ${timeStrSec(OrbitalKiller.timeMsec)}"
      if (OrbitalKiller.timeMultiplier != OrbitalKiller.realtime) {
        if(OrbitalKiller._set_stop_time) {
          strings_with_stop_moment(1) = s"[gОстановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * OrbitalKiller.base_dt * 1000).toLong)} (${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * OrbitalKiller.base_dt * 1000 / (1f * OrbitalKiller.timeMultiplier / 63 * OrbitalKiller.ticks)).toLong)})]"
        } else {
          strings_with_stop_moment(1) = s"Остановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * OrbitalKiller.base_dt * 1000).toLong)} (${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * OrbitalKiller.base_dt * 1000 / (1f * OrbitalKiller.timeMultiplier / 63 * OrbitalKiller.ticks)).toLong)})"
        }
      } else {
        if(OrbitalKiller._set_stop_time) {
          strings_with_stop_moment(1) = s"[gОстановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * OrbitalKiller.base_dt * 1000).toLong)}]"
        } else {
          strings_with_stop_moment(1) = s"Остановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * OrbitalKiller.base_dt * 1000).toLong)}"
        }
      }
      strings = strings_with_stop_moment
    } else {
      strings_without_stop_moment(0) = s"Время: $time_acceleration ${timeStrSec(OrbitalKiller.timeMsec)}"
      strings = strings_without_stop_moment
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "T"
}
