package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.components.OrbitalComponentsAware

/**
  * Created by andrey on 1/7/18.
  */
trait OrbitAction extends OrbitalComponentsAware {
  this: ScageScreenAppDMT =>

  actionDynamicPeriodIgnorePause(500 / orbitalComponents.timeMultiplier.timeMultiplier) {
    orbitalComponents.realTrajectory.continue()
    //RealTrajectory2.continue()
    //RealTrajectory3.continue()
  }

  private def nextStep() {
    (1 to orbitalComponents.timeMultiplier.timeMultiplier).foreach(step => {
      orbitalComponents.shipComponents.ships.foreach(s => {
        s.beforeStep()
      })
      orbitalComponents.system_evolution.step()
      orbitalComponents.shipComponents.ships.foreach(s => {
        s.afterStep(orbitalComponents.systemEvolutionComponents.timeMsec)
      })
      if (orbitalComponents._stop_after_number_of_tacts > 0) {
        orbitalComponents._stop_after_number_of_tacts -= 1
        if (orbitalComponents._stop_after_number_of_tacts <= 0) {
          if (orbitalComponents.timeMultiplier.timeMultiplier != realtime) {
            orbitalComponents.timeMultiplier.timeMultiplier = realtime
          }
          pause()
        }
      }
    })
  }

  nextStep()

  action {
    nextStep()
  }

  orbitalComponents.orbitsUpdater.update()

  actionDynamicPeriodIgnorePause(1000 / orbitalComponents.timeMultiplier.timeMultiplier) {
    if ( /*drawMapMode && (*/ !onPause || orbitalComponents.orbitsUpdater.needUpdateOrbits /*)*/ ) {
      orbitalComponents.orbitsUpdater.update()
    }
  }

  actionStaticPeriodIgnorePause(10000) {
    if (orbitalComponents.timeMultiplier.timeMultiplier != realtime &&
      orbitalComponents.timeMultiplier.timeMultiplier > 1f * orbitalComponents.timeMultiplier.timeMultiplier / 63 * ticks + 20) {
      println("updating timeMultiplier")
      orbitalComponents.timeMultiplier.timeMultiplier = (orbitalComponents.timeMultiplier.timeMultiplier * 1f / 63 * ticks).toInt
    }
  }
}
