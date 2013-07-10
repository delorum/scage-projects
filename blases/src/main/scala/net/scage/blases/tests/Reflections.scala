package net.scage.blases.tests

import net.scage.ScageLib._
import collection.mutable.ArrayBuffer

object Reflections extends ScageScreenApp("Reflections", 640, 480) {
  def lineFormula(v1: DVec, v2: DVec): (Double, Double) = {
    val DVec(x1, y1) = v1
    val DVec(x2, y2) = v2
    val a = (y2 - y1) / (x2 - x1)
    val b = y1 - a * x1
    (a, b)
  }

  def interpoints(l1: DVec, l2: DVec): List[DVec] = {
    val (a, b) = lineFormula(l1, l2)
    val d = 400 * a * a - 16 * b * b + 1600

    val x1 = (-2 * a * b + math.sqrt(d)) / (8 + 2 * a * a)
    val y1 = a * x1 + b

    val x2 = (-2 * a * b - math.sqrt(d)) / (8 + 2 * a * a)
    val y2 = a * x2 + b

    List(DVec(x1, y1), DVec(x2, y2))
  }

  def tangentLine(p: DVec): (DVec, DVec) = {
    val m = -4f * p.x / p.y
    val b = p.y - m * p.x
    (DVec(p.x - 5, m * (p.x - 5) + b), DVec(p.x + 5, m * (p.x + 5) + b))
  }

  def reflectLine(l1: DVec, l2: DVec): (DVec, DVec) = {
    val (sl1, sl2) = tangentLine(l2)
    val to_dir = l2 - l1
    val tan_dir = sl2 - sl1
    val norm_dir = tan_dir.rotateDeg(90)
    val to_dir_x = to_dir * tan_dir
    val to_dir_y = to_dir * norm_dir
    val from_dir = tan_dir * to_dir_x - norm_dir * to_dir_y
    (l2, l2 + from_dir)
  }

  def reflections(res: List[DVec]): List[DVec] = {
    if (-0.01 <= res.head.x && res.head.x <= 0.01 &&
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
    } yield (Vec(x, math.sqrt(100f - 4f * x * x)))) ++
      (for {
        x <- (5f to -5f by -0.1f)
      } yield (Vec(x, -math.sqrt(100f - 4f * x * x))))

  center = Vec.zero
  globalScale = 15

  val beams_i = reflections(List(DVec(1.4, -9.6), DVec(0.0, 10.1))).reverse.iterator
  val beams = ArrayBuffer[Vec](beams_i.next().toVec, beams_i.next().toVec)
  action(100) {
    if (beams_i.hasNext) beams += beams_i.next().toVec
    else {
      println(beams.length)
      deleteSelf()
    }
  }

  render {
    drawSlidingLines(ellipse, WHITE)
    drawSlidingLines(beams, RED)
  }
}
