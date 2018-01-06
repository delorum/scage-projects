package com.github.dunnololda.scageprojects.orbitalkiller.util

import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder

trait TimeStringUtils {
  def timeStrSec(time_msec: Long, add_plus_sign: Boolean = false): String = {
    val is_below_zero = time_msec < 0
    val abs_time_msec = scala.math.abs(time_msec)
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
    val abs_time_msec = scala.math.abs(time_msec)
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

trait DistanceStringUtils {
  def mOrKmOrMKm(meters: Number): String = {
    val d = meters.doubleValue()
    val absd = scala.math.abs(d)
    if (absd < 1000) {
      f"$d%.2f м"
    } else if (absd < 1000000) {
      f"${d / 1000}%.2f км"
    } else if (absd < 1000000000) {
      f"${d / 1000000}%.2f тыс. км"
    } else {
      f"${d / 1000000000}%.2f млн. км"
    }
  }
}

trait VelocityStringUtils {
  def msecOrKmsec(msec: Number): String = {
    InterfaceHolder.msecOrKmH.selectedVariant match {
      // TODO избавиться!
      case 0 => // m/sec
        if (scala.math.abs(msec.doubleValue()) < 1000) {
          f"${msec.doubleValue()}%.2f м/сек"
        } else {
          f"${msec.doubleValue() / 1000}%.2f км/сек"
        }
      case 1 => // km/h
        f"${msec.doubleValue() * 3.6}%.2f км/ч"
      case _ =>
        ""
    }
  }
}

trait AccelerationStringUtils {
  /**
    * Ускорение
    *
    * @param msec - метры в секунду
    * @return
    */
  def msec2OrKmsec2(msec: Number): String = {
    if (scala.math.abs(msec.doubleValue()) < 1000) f"${msec.doubleValue()}%.2f м/сек^2" else f"${msec.doubleValue() / 1000}%.2f км/сек^2"
  }
}

trait ForceStringUtils {
  def newtonOrKilonewton(newton: Number): String = {
    if (scala.math.abs(newton.doubleValue()) < 1000) f"${newton.doubleValue()}%.2f Н" else f"${newton.doubleValue() / 1000}%.2f кН"
  }

  def gOrKg(kg: Double): String = {
    if (kg < 1) f"${kg * 1000}%.0f г"
    else {
      if (kg % 1 == 0) f"$kg%.0f кг"
      else f"${kg.toInt} кг ${(kg - kg.toInt) * 1000}%.0f г"
    }
  }
}

/**
  * Created by andrey on 1/6/18.
  */
object StringUtils extends TimeStringUtils with DistanceStringUtils with VelocityStringUtils with ForceStringUtils
  with AccelerationStringUtils
