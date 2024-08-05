package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scage.support.messages.ScageMessage
import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceSwitcher, RealTrajectory}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main.interfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interfaces.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._

class RealTrajectorySwitcher extends InterfaceSwitcher {
  private def enabledRealTrajectorySwitcher = if(interfaceHolder.realTrajectorySwitcher.numPoints > RealTrajectory.curPoints) {
    s"RT\\[${timeStrSec(RealTrajectory.curPoints*1000)}/${timeStrSec(numPoints*1000)}\\]"
  } else {
    s"RT\\[${timeStrSec(numPoints*1000)}\\]"
  }
  override def strVariants: Array[String] = Array("RToff", enabledRealTrajectorySwitcher)

  override def selectedStrVariantLen = ScageMessage.messageBounds(selectedStrVariant).ix

  def showRealTrajectory:Boolean = selectedVariant != 0

  var numPoints:Long = 24*3600

  override def switchForward(): Unit = {
    super.switchForward()
    Main.needToUpdateRealTrajectory("changed central body")
  }

  override def switchBack(): Unit = {
    super.switchBack()
    Main.needToUpdateRealTrajectory("changed central body")
  }
}
