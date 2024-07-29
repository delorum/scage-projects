package com.github.dunnololda.scageprojects.orbitalkiller.util

/**
  * Created by andrey on 1/6/18.
  */
object TimeUtils {
  def timeStrSec(time_msec: Long, add_plus_sign: Boolean = false): String = {
    val is_below_zero = time_msec < 0
    val abs_time_msec = math.abs(time_msec)
    val result = if (abs_time_msec < 1000) s"0 сек."
    else {
      val sec = 1000l
      val min = sec * 60
      val hour = min * 60
      val day = hour * 24

      List(
        (abs_time_msec / day, "д."),
        (abs_time_msec % day / hour, "ч."),
        (abs_time_msec % hour / min, "мин."),
        (abs_time_msec % min / sec, "сек.")
      ).filter(_._1 > 0).map(e => e._1 + " " + e._2).mkString(" ")
    }
    if (is_below_zero) s"-$result"
    else {
      if (add_plus_sign) s"+$result"
      else result
    }
  }

  def timeStrMsec(time_msec: Long, add_plus_sign: Boolean = false): String = {
    val is_below_zero = time_msec < 0
    val abs_time_msec = math.abs(time_msec)
    val result = if (abs_time_msec < 1000) s"$abs_time_msec мсек."
    else {
      val sec = 1000l
      val min = sec * 60
      val hour = min * 60
      val day = hour * 24

      List(
        (abs_time_msec / day, "д."),
        (abs_time_msec % day / hour, "ч."),
        (abs_time_msec % hour / min, "мин."),
        (abs_time_msec % min / sec, "сек."),
        (abs_time_msec % sec, "мсек.")
      ).filter(_._1 > 0).map(e => e._1 + " " + e._2).mkString(" ")
    }
    if (is_below_zero) s"-$result"
    else {
      if (add_plus_sign) s"+$result"
      else result
    }
  }
}
