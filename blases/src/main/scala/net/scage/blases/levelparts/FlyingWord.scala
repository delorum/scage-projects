package net.scage.blases.levelparts

import net.scage.support.{Vec, ScageColor}
import net.scage.ScageLib._
import net.scage.blases.Blases._

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) {
  private var lifetime = 100;
  private val dir = direction.n
  private var coord = init_coord

  actionNoPause {
    if(lifetime > 0) {
      coord += dir
      lifetime -= 1
    } else deleteSelf()
  }

  render {
    if(lifetime > 0) {
      print(message, coord, color)
    } else deleteSelf()
  }
}
