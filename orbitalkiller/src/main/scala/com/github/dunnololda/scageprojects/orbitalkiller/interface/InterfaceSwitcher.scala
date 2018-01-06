package com.github.dunnololda.scageprojects.orbitalkiller.interface

import com.github.dunnololda.scage.support.messages.ScageMessage

abstract class InterfaceSwitcher {
  def strVariants: Array[String]

  private lazy val strVariantsLengthes = strVariants.map(s => ScageMessage.messageBounds(s).ix)

  protected var selected_variant = 0

  def selectedVariant = selected_variant

  def selectedStrVariant = strVariants(selected_variant)

  def selectedStrVariantLen = strVariantsLengthes(selected_variant)

  def switchForward(): Unit = {
    selected_variant += 1
    if (selected_variant > strVariants.length - 1) {
      selected_variant = 0
    }
  }

  def switchBack(): Unit = {
    selected_variant -= 1
    if (selected_variant < 0) {
      selected_variant = strVariants.length - 1
    }
  }

  def active: Boolean = true
}
