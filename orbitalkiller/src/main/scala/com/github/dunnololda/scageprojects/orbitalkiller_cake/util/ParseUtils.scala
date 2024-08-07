package com.github.dunnololda.scageprojects.orbitalkiller_cake.util

import com.github.dunnololda.scage.ScageLibD.DVec

object ParseUtils {

  def safeParseDVec(str: String): Option[DVec] = {
    val s = str.split(":")
    if (s.length == 2) {
      try {
        Some(DVec(s(0).toDouble, s(1).toDouble))
      } catch {
        case _: Exception => None
      }
    } else None
  }

  def safeParseLong(str: String): Option[Long] = {
    try {
      Some(str.toLong)
    } catch {
      case _: Exception => None
    }
  }

  def safeParseInt(str: String): Option[Int] = {
    try {
      Some(str.toInt)
    } catch {
      case _: Exception => None
    }
  }

  def safeParseDouble(str: String): Option[Double] = {
    try {
      Some(str.toDouble)
    } catch {
      case _: Exception => None
    }
  }
}
