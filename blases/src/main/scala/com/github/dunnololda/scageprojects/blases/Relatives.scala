package com.github.dunnololda.scageprojects.blases

import com.github.dunnololda.scage.ScageLib._

object Relatives {
  def rVec(x:Float, y:Float):Vec = Vec((x/1024f*windowWidth).toInt, (y/768f*windowHeight).toInt)
  def rInt(i:Int):Int = (i*windowWidth/1024f).toInt
  def rFloat(f:Float):Float = f*windowWidth/1024f
  def rVec(vec: Vec): Vec = Vec((vec.x/1024f*windowWidth).toInt, (vec.y/768f*windowHeight).toInt)
  def rColor(color:ScageColor) = if(!Blases.onPause) color else DARK_GRAY
}
