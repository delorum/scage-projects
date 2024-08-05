package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{Box, Circle, Line, Polygon, DynamicShape => Phys2dShape}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.Phys2dUtils.DVec2DoublePhys2dVector

sealed trait Shape {
  def aabb(center: DVec, rotation: Double): AABB

  def phys2dShape: Phys2dShape

  def wI: Double
}

object Shape {
  case class CircleShape(radius: Double) extends Shape {
    def aabb(center: DVec, rotation: Double): AABB = {
      AABB(center, radius * 2, radius * 2)
    }

    def phys2dShape: Phys2dShape = new Circle(radius)

    lazy val wI: Double = radius * radius / 2.0
  }

  /*
  * Due to lack of line colliding algorithms, bodies with line shapes may be static only. If you want dynamic, use boxes
  * */
  case class LineShape(to: DVec) extends Shape {
    /*val center = from + (to - from)/2.0

    /**
     * Get the closest point on the line to a given point
     */
    def closestPoint(point:Vec):Vec = {
      val vec = to - from
      val loc = point - from
      val v = vec.n
      val proj = loc.project(v)
      if(proj.norma2 > vec.norma2) to
      else {
        val proj2 = proj + from
        val other = proj2 - to
        if(other.norma2 > vec.norma2) from
        else proj2
      }
    }

    def distanceSquared(point:Vec):Double = closestPoint(point).dist2(point)

    val vec = to - from
    def dx = vec.x
    def dy = vec.y*/

    def aabb(from: DVec, rotation: Double): AABB = {
      val to2 = from + to.rotateDeg(rotation)
      val center = from + (to2 - from) / 2.0
      AABB(center, math.max(math.abs(to2.x - from.x), 5.0), math.max(math.abs(to2.y - from.y), 5.0))
    }

    def phys2dShape: Phys2dShape = new Line(to.x, to.y)

    lazy val wI: Double = to.norma2 / 12.0
  }

  case class BoxShape(width: Double, height: Double) extends Shape {
    val radius: Double = math.sqrt(width * width + height * height)

    def aabb(center: DVec, rotation: Double): AABB = {
      /*val one = center + DVec(-width/2, height/2).rotateDeg(rotation)
      val two = center + DVec(width/2, height/2).rotateDeg(rotation)
      val three = center + DVec(width/2, -height/2).rotateDeg(rotation)
      val four = center + DVec(-width/2, -height/2).rotateDeg(rotation)
      val points = List(one, two, three, four)
      val xs = points.map(p => p.x)
      val ys = points.map(p => p.y)
      val min_x = xs.min
      val max_x = xs.max
      val min_y = ys.min
      val max_y = ys.max
      AABB(center, max_x - min_x, max_y - min_y)*/
      AABB(center, radius, radius)
    }

    def phys2dShape: Phys2dShape = new Box(width, height)

    lazy val wI: Double = (width * width + height * height) / 12.0
  }

  case class PolygonShape(points: List[DVec], convex_parts: List[PolygonShape]) extends Shape {
    val radius: Double = math.sqrt(points.map(p => p.norma2).max) * 2
    val points_center: DVec = points.sum / points.length
    val points_radius: Double = math.sqrt(points.map(p => (p - points_center).norma2).max) * 2

    def aabb(center: DVec, rotation: Double): AABB = {
      AABB(center, radius, radius)
    }

    def phys2dShape: Phys2dShape = new Polygon(points.map(_.toPhys2dVecD).toArray)

    lazy val wI: Double = {
      val numerator = (for {
        n <- 0 until points.length - 2
        p_n_plus_1 = points(n + 1)
        p_n = points(n)
      } yield p_n_plus_1.*/(p_n) * (p_n_plus_1.norma2 + (p_n_plus_1 * p_n) + p_n.norma2)).sum
      val denominator = (for {
        n <- 0 until points.length - 2
        p_n_plus_1 = points(n + 1)
        p_n = points(n)
      } yield p_n_plus_1.*/(p_n)).sum
      numerator / denominator / 6.0
    }

    lazy val area: Double = math.abs(0.5*(points ::: List(points.head)).sliding(2).map {
      case List(p, p1) => (p.x + p1.x)*(p.y - p1.y)
    }.sum)
  }
}
