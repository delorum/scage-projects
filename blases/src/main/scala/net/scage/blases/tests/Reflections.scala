package net.scage.blases.tests

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.Vec
import collection.mutable.ArrayBuffer

object Reflections extends ScageScreenApp("Reflections", 640, 480) {
  def lineFormula(v1:Vec, v2:Vec):(Float, Float) = {
    val Vec(x1, y1) = v1
    val Vec(x2, y2) = v2
    val a = (y2 - y1)/(x2 - x1)
    val b = y1 - a*x1
    (a, b)
  }

  def interpoints(l1:Vec, l2:Vec):List[Vec] = {
    val (a, b) = lineFormula(l1, l2)
    val d = 400*a*a - 16*b*b + 1600
    /*if(d < 0) Nil
    else {*/
      val x1 = (-2*a*b + math.sqrt(d))/(8 + 2*a*a)
      val y1 = a*x1 + b

      val x2 = (-2*a*b - math.sqrt(d))/(8 + 2*a*a)
      val y2 = a*x2 + b

      List(Vec(x1, y1), Vec(x2,y2))
    /*}*/
  }

  def tangentLine(p:Vec):(Vec, Vec) = {
    val m = -4f*p.x/p.y
    val b = p.y - m*p.x
    (Vec(p.x-5, m*(p.x-5)+b), Vec(p.x+5, m*(p.x+5)+b))
  }

  def reflectLine(l1:Vec, l2:Vec):(Vec, Vec) = {
    val (sl1, sl2) = tangentLine(l2)
    val to_dir = l2 - l1
    val tan_dir = sl2 - sl1
    val norm_dir = tan_dir.rotateDeg(90)
    val to_dir_x = to_dir*tan_dir
    val to_dir_y = to_dir*norm_dir
    val from_dir = tan_dir*to_dir_x - norm_dir*to_dir_y
    (l2, l2 + from_dir)
  }

  def reflections(res:List[Vec]):List[Vec] = {
    println(res.head)
    if(-0.01 <= res.head.x && res.head.x <= 0.01 &&
           8 <= res.head.y && res.head.y <= 12) res
    else {
      val (l1, l2) = reflectLine(res.tail.head, res.head)
      val next = interpoints(l1, l2).find {
        case p => p.dist(res.head) > 1
      }.get
      reflections(next :: res)
    }
  }

  val ellipse =
    (for {
      x <- (-5f to 5f by 0.1f)
    } yield (Vec(x, math.sqrt(100f - 4f*x*x)))) ++
    (for {
      x <- (5f to -5f by -0.1f)
    } yield (Vec(x, -math.sqrt(100f - 4f*x*x))))

  center = Vec.zero
  globalScale = 15

  val beams = reflections(List(Vec(1.4, -9.6), Vec(0.0, 10.1))).reverse
  println(beams.length)

  render {
    ellipse.toList.sliding(2).foreach {
      case List(a, b) => drawLine(a, b, WHITE)
    }
    beams./*take(11).*/sliding(2).foreach {
      case List(a, b) => drawLine(a,b, RED)
    }
    /*val tangents = beams.flatMap(p => {
      val (sl1, sl2) = tangentLine(p)
      val normal = (sl2-sl1).rotateDeg(90).n
      List(sl1, sl2, p - normal*5, p + normal*5)
    })
    drawLines(tangents, WHITE)*/
  }
}
