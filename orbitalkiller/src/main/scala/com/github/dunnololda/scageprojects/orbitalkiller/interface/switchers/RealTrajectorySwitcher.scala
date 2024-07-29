package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scage.support.messages.ScageMessage
import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceHolder, RealTrajectory, OrbitalKiller, InterfaceSwitcher}

class RealTrajectorySwitcher extends InterfaceSwitcher {
  private def enabledRealTrajectorySwitcher = if(InterfaceHolder.realTrajectorySwitcher.numPoints > RealTrajectory.curPoints) {
    s"RT\\[${com.github.dunnololda.scageprojects.orbitalkiller.timeStrSec(RealTrajectory.curPoints*1000)}/${com.github.dunnololda.scageprojects.orbitalkiller.timeStrSec(numPoints*1000)}\\]"
  } else {
    s"RT\\[${com.github.dunnololda.scageprojects.orbitalkiller.timeStrSec(numPoints*1000)}\\]"
  }
  override def strVariants: Array[String] = Array("RToff", enabledRealTrajectorySwitcher)

  override def selectedStrVariantLen = ScageMessage.messageBounds(selectedStrVariant).ix

  def showRealTrajectory:Boolean = selectedVariant != 0

  var numPoints:Long = 24*3600

  override def switchForward(): Unit = {
    super.switchForward()
    OrbitalKiller.needToUpdateOrbits("changed central body")
  }

  override def switchBack(): Unit = {
    super.switchBack()
    OrbitalKiller.needToUpdateOrbits("changed central body")
  }
}
