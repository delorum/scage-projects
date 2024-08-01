package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceElement, timeStrSec}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.{Constants, Main}

class TimeInfo extends InterfaceElement {
  private val strings_without_stop_moment = Array("")
  private val strings_with_stop_moment = Array("", "")
  private var strings = strings_without_stop_moment

  override protected def _update(): Unit = {
    val time_acceleration = f"x${(Main.timeMultiplier * Constants.k).toInt} (${1f * Main.timeMultiplier / 63 * Main.tps}%.2f)"
    if (Main._stop_after_number_of_tacts > 0 || Main._set_stop_time) {
      strings_with_stop_moment(0) = s"Время: $time_acceleration ${timeStrSec(Main.system_evolution.timeMsec)}"
      if (Main.timeMultiplier != Constants.realtime) {
        if(Main._set_stop_time) {
          strings_with_stop_moment(1) = s"[gОстановка через ${timeStrSec((Main._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)} (${timeStrSec((Main._stop_after_number_of_tacts * Constants.base_dt * 1000 / (1f * Main.timeMultiplier / 63 * Main.tps)).toLong)})]"
        } else {
          strings_with_stop_moment(1) = s"Остановка через ${timeStrSec((Main._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)} (${timeStrSec((Main._stop_after_number_of_tacts * Constants.base_dt * 1000 / (1f * Main.timeMultiplier / 63 * Main.tps)).toLong)})"
        }
      } else {
        if(Main._set_stop_time) {
          strings_with_stop_moment(1) = s"[gОстановка через ${timeStrSec((Main._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)}]"
        } else {
          strings_with_stop_moment(1) = s"Остановка через ${timeStrSec((Main._stop_after_number_of_tacts * Constants.base_dt * 1000).toLong)}"
        }
      }
      strings = strings_with_stop_moment
    } else {
      strings_without_stop_moment(0) = s"Время: $time_acceleration ${timeStrSec(Main.system_evolution.timeMsec)}"
      strings = strings_without_stop_moment
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "T"
}
