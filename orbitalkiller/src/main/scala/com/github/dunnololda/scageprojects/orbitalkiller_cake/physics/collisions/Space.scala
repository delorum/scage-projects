package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.MutableBodyState

class Space(val bodies: Seq[MutableBodyState], val center: DVec, val width: Double, val height: Double) {
  def this(bodies: Seq[MutableBodyState], center: DVec) = {
    this(bodies, center, {
      val (init_min_x, init_max_x) = {
        bodies.headOption.map(b => {
          val AABB(c, w, _) = b.aabb
          (c.x - w / 2, c.x + w / 2)
        }).getOrElse((0.0, 0.0))
      }
      val (min_x, max_x) = bodies.foldLeft((init_min_x, init_max_x)) {
        case ((res_min_x, res_max_x), b) =>
          val AABB(c, w, _) = b.aabb
          val new_min_x = c.x - w / 2
          val new_max_x = c.x + w / 2
          (
            if (new_min_x < res_min_x) new_min_x else res_min_x,
            if (new_max_x > res_max_x) new_max_x else res_max_x
          )
      }
      math.max(math.abs(max_x - center.x) * 2, math.abs(center.x - min_x) * 2)
    }, {
      val (init_min_y, init_max_y) = {
        bodies.headOption.map(b => {
          val AABB(c, _, h) = b.aabb
          (c.y - h / 2, c.y + h / 2)
        }).getOrElse((0.0, 0.0))
      }
      val (min_y, max_y) = bodies.foldLeft(init_min_y, init_max_y) {
        case ((res_min_y, res_max_y), b) =>
          val AABB(c, _, h) = b.aabb
          val new_min_y = c.y - h / 2
          val new_max_y = c.y + h / 2
          (
            if (new_min_y < res_min_y) new_min_y else res_min_y,
            if (new_max_y > res_max_y) new_max_y else res_max_y
          )
      }
      math.max(math.abs(max_y - center.y) * 2, math.abs(center.y - min_y) * 2)
    })
  }


  lazy val aabb: AABB = AABB(center, width, height)

  lazy val quadSpaces: List[Space] = {
    val AABB(c, w, h) = aabb

    val c1 = c + DVec(-w / 4, -h / 4)
    val aabb1 = AABB(c1, w / 2, h / 2)
    val bodies1 = bodies.filter(b => b.aabb.aabbCollision(aabb1))

    val c2 = c + DVec(-w / 4, h / 4)
    val aabb2 = AABB(c2, w / 2, h / 2)
    val bodies2 = bodies.filter(b => b.aabb.aabbCollision(aabb2))

    val c3 = c + DVec(w / 4, h / 4)
    val aabb3 = AABB(c3, w / 2, h / 2)
    val bodies3 = bodies.filter(b => b.aabb.aabbCollision(aabb3))

    val c4 = c + DVec(w / 4, -h / 4)
    val aabb4 = AABB(c4, w / 2, h / 2)
    val bodies4 = bodies.filter(b => b.aabb.aabbCollision(aabb4))

    List(
      new Space(bodies1, c1, w / 2, h / 2),
      new Space(bodies2, c2, w / 2, h / 2),
      new Space(bodies3, c3, w / 2, h / 2),
      new Space(bodies4, c4, w / 2, h / 2)
    )
  }
}

object Space {
  /**
   * Реализация http://en.wikipedia.org/wiki/Quadtree
   * @param space - начальное пространство, которое будем или не будем разделять
   * @param max_level - желаемое максимальное количество разбиений области на подобласти
   * @param target - желаемое максимальное количество объектов в одной области
   * @param level - текущий уровень разделения
   * @param spaces - список пространств, который отдадим в качестве результата
   * @return
   */
  def splitSpace(space: Space, max_level: Int, target: Int, level: Int = 0, spaces: List[Space] = Nil): List[Space] = {
    if (space.bodies.length <= target) space :: spaces
    else if (level > max_level) space :: spaces
    else {
      space.quadSpaces.flatMap(s => splitSpace(s, max_level, target, level + 1, spaces))
    }
  }
}
