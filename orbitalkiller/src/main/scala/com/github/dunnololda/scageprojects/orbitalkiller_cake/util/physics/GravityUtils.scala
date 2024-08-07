package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState

object GravityUtils {
  // гравитационная постоянная
  val G: Double = 6.6742867e-11

  def gravityForce(body1_coord: DVec, body1_mass: Double, body2_coord: DVec, body2_mass: Double): DVec = {
    (body1_coord - body2_coord).n * G * body1_mass * body2_mass / body1_coord.dist2(body2_coord)
  }

  def equalGravityRadius(planet1: MutableBodyState, planet2: MutableBodyState): Double = {
    val A = planet1.coord.dist(planet2.coord)
    val X = planet1.mass / planet2.mass
    A * math.sqrt(X) / (math.sqrt(X) + 1)
  }

  /**
   * soi - sphere of influence. Радиус сферы вокруг планеты, в которой она оказывает наибольшее гравитационное влияние
   * @param smaller_planet_mass - масса малой планеты
   * @param semi_major_axis - главная полуось орбиты малой планеты
   * @param bigger_planet_mass - масса большей планеты или звезды, вокруг которой вращается малая планета
   * @return
   */
  def soi(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    semi_major_axis * math.pow(smaller_planet_mass / bigger_planet_mass, 2.0 / 5)
  }

  def halfHillRadius(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    0.5 * semi_major_axis * math.pow(smaller_planet_mass / (3 * bigger_planet_mass), 1.0 / 3)
  }

  /**
   * Возвращает информацию о небесном теле, в сфере влияния которого находится наш объект (заведомо гораздо меньшей массы).
   * Мы вычисляем это, определяя в сфере Хилла какого небесного тела мы находимся. Потенциальные кандидаты передаются в аргументе
   * planet_state, и они там отсортированы по возрастанию массы. Мы проверяем нахождение в сфере Хилла начиная с самого малого.
   *
   * @param ship_coord - позиция нашего объекта
   * @param ship_mass - масса нашего объекта
   * @param planet_states - информация о небесных телах, в сфере влияния которых потенциально мы можем быть. Это список, и он должен быть
   *                      отсортирован по возрастанию массы. В конце списка должно быть Солнце!
   * @return
   */
  def insideSphereOfInfluenceOfCelestialBody(
      ship_coord: DVec,
      ship_mass: Double,
      planet_states: Seq[(CelestialBody, MutableBodyState)]): Option[(CelestialBody, MutableBodyState)] = {
    if (planet_states.isEmpty) None
    else if (planet_states.length == 1) Some(planet_states.head)
    else {
      val x = planet_states.find { case (smaller_planet, smaller_planet_state) =>
        ship_coord.dist2(smaller_planet_state.coord) <= smaller_planet.half_hill_radius2
      }
      if (x.nonEmpty) x else Some(planet_states.last)
    }
  }
}
