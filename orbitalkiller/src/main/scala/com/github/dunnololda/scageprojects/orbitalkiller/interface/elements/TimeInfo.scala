package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceElement, OrbitalKiller, _}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Constants

class TimeInfo extends InterfaceElement {
  private val strings_without_stop_moment = Array("")
  private val strings_with_stop_moment = Array("", "")
  private var strings = strings_without_stop_moment

  override protected def _update(): Unit = {
    val time_acceleration = f"x${(OrbitalKiller.timeMultiplier * Constants.k).toInt} (${1f * OrbitalKiller.timeMultiplier / 63 * OrbitalKiller.tps}%.2f)"
    if (OrbitalKiller._stop_after_number_of_tacts > 0 || OrbitalKiller._set_stop_time) {
      strings_with_stop_moment(0) = s"Время: $time_acceleration ${timeStrSec(OrbitalKiller.system_evolution.timeMsec)}"
      if (OrbitalKiller.timeMultiplier != Constants.realtime) {
        if(OrbitalKiller._set_stop_time) {
          strings_with_stop_moment(1) = s"[gОстановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)} (${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * Constants.base_dt * 1000 / (1f * OrbitalKiller.timeMultiplier / 63 * OrbitalKiller.tps)).toLong)})]"
        } else {
          strings_with_stop_moment(1) = s"Остановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)} (${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * Constants.base_dt * 1000 / (1f * OrbitalKiller.timeMultiplier / 63 * OrbitalKiller.tps)).toLong)})"
        }
      } else {
        if(OrbitalKiller._set_stop_time) {
          strings_with_stop_moment(1) = s"[gОстановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)}]"
        } else {
          strings_with_stop_moment(1) = s"Остановка через ${timeStrSec((OrbitalKiller._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)}"
        }
      }
      strings = strings_with_stop_moment
    } else {
      strings_without_stop_moment(0) = s"Время: $time_acceleration ${timeStrSec(OrbitalKiller.system_evolution.timeMsec)}"
      strings = strings_without_stop_moment
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "T"
}
