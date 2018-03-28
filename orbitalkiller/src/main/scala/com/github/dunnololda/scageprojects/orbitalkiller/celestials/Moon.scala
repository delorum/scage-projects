package com.github.dunnololda.scageprojects.orbitalkiller.celestials

import com.github.dunnololda.scage.ScageLib.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents.moonIndex

/**
  * TODO
  *
  * @author aborunov
  */
class Moon(moon_start_position: DVec,
           moon_init_velocity: DVec,
           earth: Earth) extends Planet(
  moonIndex, "Луна",
  mass = 7.3477E22,
  init_coord = moon_start_position,
  init_velocity = moon_init_velocity,
  init_ang_vel = 360.0 / (26l * 24 * 60 * 60 + 8l * 60 * 60 + 59l * 60 + 44), // период орбиты луны в данной симуляции: 26 д. 8 ч. 59 мин. 44 сек, равен периоду обращения вокруг собственной оси
  radius = 1737000,
  earth, 2000)
