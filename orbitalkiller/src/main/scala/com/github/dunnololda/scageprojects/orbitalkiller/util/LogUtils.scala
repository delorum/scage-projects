package com.github.dunnololda.scageprojects.orbitalkiller.util

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller

/**
  * Created by andrey on 1/8/18.
  */
object LogUtils {
  def log(str: Any): Unit = {
    println(s"[${OrbitalKiller.msecsFromAppStart}] ${str.toString}")
  }
}
