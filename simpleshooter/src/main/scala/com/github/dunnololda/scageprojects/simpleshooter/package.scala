package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._

package object simpleshooter {
  val speed = 9f
  val bullet_speed_multiplier = 1.2f
  val bullet_count = 30
  val bullet_damage = 10
  val map_width = 800
  val map_height = 600
  val body_radius = 10
  val bullet_size = 3

  def loadMap(map_name:String):List[Wall] = {
    def tryFloat(str:String):Boolean = {
      try {
        val f = str.toFloat
        true
      } catch {
        case e:Exception => false
      }
    }

    try {
      (for {
        line <- io.Source.fromFile(map_name).getLines()
        if !line.startsWith("#")
        coords = line.split(" ")
        if coords.length == 4 && coords.forall(c => tryFloat(c))
      } yield Wall(Vec(coords(0).toFloat, coords(1).toFloat), Vec(coords(2).toFloat, coords(3).toFloat))).toList
    } catch {
      case e:Exception => Nil
    }
  }

  def isCoordNearWall(coord:Vec, wall:Wall, body_radius:Float):Boolean = {
    val one = (wall.to - wall.from).rotateDeg(135).n*body_radius + wall.from
    val two = (wall.to - wall.from).rotateDeg(-135).n*body_radius + wall.from
    val three = (wall.from - wall.to).rotateDeg(135).n*body_radius + wall.to
    val four = (wall.from - wall.to).rotateDeg(-135).n*body_radius + wall.to
    val area = List(one, two, three, four)
    coordOnArea(coord, area)
  }

  def isCoordCorrect(coord:Vec, body_radius:Float, walls:Seq[Wall]):Boolean = {
    walls.forall(w => !isCoordNearWall(coord, w, body_radius))
  }

  def randomCoord(width:Float, height:Float, body_radius:Float, walls:Seq[Wall]):Vec = {
    val coord = Vec(math.random*width, math.random*height)
    if (isCoordCorrect(coord, body_radius, walls)) coord
    else randomCoord(width, height, body_radius, walls)
  }

  def isCoordVisible(coord:Vec, from:Vec, walls:Seq[Wall]):Boolean = {
    walls.forall(w => {
      !areLinesIntersect(from, coord, w.from, w.to)
    })
  }

  def isPathCorrect(from:Vec, to:Vec, body_radius:Float, walls:Seq[Wall]):Boolean = {
    walls.forall(w => !areLinesIntersect(from, to, w.from, w.to)) && isCoordCorrect(to, body_radius, walls)
  }

  def isWallVisible(wall:Wall, from:Vec, other_walls:Seq[Wall]):Boolean = {
    val middle = (wall.to - wall.from).n*(wall.to.dist(wall.from)/2) + wall.from
    isCoordVisible(wall.from, from, other_walls) || isCoordVisible(middle, from, other_walls) || isCoordVisible(wall.to, from, other_walls)
  }
}
