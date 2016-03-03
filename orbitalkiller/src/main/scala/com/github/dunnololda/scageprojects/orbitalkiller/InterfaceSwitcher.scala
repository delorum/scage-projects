package com.github.dunnololda.scageprojects.orbitalkiller

abstract class InterfaceSwitcher {
  def strVariants:Array[String]
  
  protected var selected_variant = 0
  def selectedVariant = selected_variant
  def selectedStrVariant = strVariants(selected_variant)
  
  def switch(): Unit = {
    selected_variant +=1
    if(selected_variant >= strVariants.length) {
      selected_variant = 0
    }
  }
}
