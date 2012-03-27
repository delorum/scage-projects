package net.scage.blases

import net.scage.support.Vec
import net.scage.ScageLib._

object Relatives {
  def rVec(x:Float, y:Float):Vec = Vec((x/1024f*windowWidth).toInt, (y/768f*windowHeight).toInt)
  def rInt(i:Int):Int = (i*windowWidth/1024f).toInt
  def rVec(vec: Vec): Vec = Vec((vec.x/1024f*windowWidth).toInt, (vec.y/768f*windowHeight).toInt)
}
