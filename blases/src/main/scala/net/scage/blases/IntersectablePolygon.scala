package net.scage.blases

import net.scage.support.Vec
import net.scage.blases.Blases._

trait IntersectablePolygon {
  def vertices: List[Vec]

  private lazy val vertices_zipped = if (vertices.length >= 2) {
    val vertices_shift = vertices.last :: vertices.init
    vertices_shift.zip(vertices)
  } else List[(Vec, Vec)]()

  def containsCoord(coord: Vec): Boolean = {
    def _areLinesIntersect(a: Vec, b: Vec, c: Vec, d: Vec): Boolean = {
      val common = (b.x - a.x) * (d.y - c.y) - (b.y - a.y) * (d.x - c.x)
      if (common == 0) false
      else {
        val rH = (a.y - c.y) * (d.x - c.x) - (a.x - c.x) * (d.y - c.y)
        val sH = (a.y - c.y) * (b.x - a.x) - (a.x - c.x) * (b.y - a.y)

        val r = rH / common
        val s = sH / common

        if (r >= 0 && r <= 1 && s >= 0 && s <= 1) true
        else false
      }
    }
    if (vertices.length < 2) false
    else {
      val a = coord
      val b = Vec(Integer.MAX_VALUE, coord.y)
      val intersections = vertices_zipped.foldLeft(0) {
        case (result, (c, d)) => if (_areLinesIntersect(a, b, c, d)) result + 1 else result
      }
      intersections % 2 != 0
    }
  }

  private lazy val (min_x, max_x, min_y, max_y) = vertices.map(vertice => tracer.point(vertice)).foldLeft((tracer.N_x, 0, tracer.N_y, 0)) {
    case ((current_min_x, current_max_x, current_min_y, current_max_y), vertice) =>
      val new_min_x = math.min(current_min_x, vertice.ix)
      val new_max_x = math.max(current_max_x, vertice.ix)
      val new_min_y = math.min(current_min_y, vertice.iy)
      val new_max_y = math.max(current_max_y, vertice.iy)
      (new_min_x, new_max_x, new_min_y, new_max_y)
  }

  lazy val polygon_points = (for {
    i <- min_x to max_x
    j <- min_y to max_y
    coord = tracer.pointCenter(Vec(i, j))
    if containsCoord(coord)
  } yield List(Vec(i, j), Vec(i-1, j), Vec(i, j-1), Vec(i+1, j), Vec(i, j+1))).flatten.toSet

  def blasesInside = polygon_points.map(point => tracer.tracesInPoint(point)).flatten.filter(trace => containsCoord(trace.location))
  def forEachBlaseInside(func:Blase => Any) {
    for {
      point <- polygon_points
      blases = tracer.tracesInPoint(point)
      blase <- blases
      if containsCoord(blase.location)
    } func(blase)
  }
}
