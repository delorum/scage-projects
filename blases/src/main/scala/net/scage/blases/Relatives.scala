package net.scage.blases

import net.scage.ScageLib._
import net.scage.support.{ScageColor, Vec}

object Relatives {
  def rVec(x:Float, y:Float):Vec = Vec((x/1024f*windowWidth).toInt, (y/768f*windowHeight).toInt)
  def rInt(i:Int):Int = (i*windowWidth/1024f).toInt
  def rVec(vec: Vec): Vec = Vec((vec.x/1024f*windowWidth).toInt, (vec.y/768f*windowHeight).toInt)
  def rColor(color:ScageColor) = if(!Blases.onPause) color else DARK_GRAY
}
