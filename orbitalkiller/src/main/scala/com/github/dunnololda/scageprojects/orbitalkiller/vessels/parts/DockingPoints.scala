package com.github.dunnololda.scageprojects.orbitalkiller.vessels.parts

import com.github.dunnololda.scage.ScageLibD.Double2DVecrich
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller.vessels.PolygonShip

class DockingPoints(val p1: DVec,
                    val p2: DVec,
                    ship: PolygonShip,
                    val disabled_engine: Option[DisabledEngine],
                    val ordered_hull: List[DVec]) {
  val index = ScageId.nextId
  val joint_point = p1 + (p2 - p1) * 0.5
  val dock_dir = joint_point.n
  val dock_dist = 0.5

  // в метрах, при каком расстоянии между точками стыковки двух кораблей происходит захватю Для простоты это значение - одинаковая для всех константа. Вынесли сюда, чтобы было одно место, где поменять.
  def curP1 = ship.currentState.coord + p1.rotateDeg(ship.currentState.ang)

  def curP1vel = ship.currentState.vel + (ship.currentState.ang_vel * p1.rotateDeg(90))

  def curP2 = ship.currentState.coord + p2.rotateDeg(ship.currentState.ang)

  def curP2vel = ship.currentState.vel + (ship.currentState.ang_vel * p2.rotateDeg(90))

  /**
    * Стыковочные точки находятся на достаточном расстоянии друг от друга, чтобы состыковаться
    *
    * @param other_ship_docking_points - стыковочные точки другого корабля
    * @return
    */
  def pointsMatch(other_ship_docking_points: DockingPoints): Boolean = {
    curP1.dist(other_ship_docking_points.curP1) < dock_dist && curP2.dist(other_ship_docking_points.curP2) < dock_dist
  }

  /**
    * Наши стыковочные точки лежат на линиях стыковки. Если дальше двигаться в сторону точек стыковки другого корабля, то состыкуемся
    *
    * @param dp - стыковочные точки другого корабля
    * @return
    */
  def pointsOnTheRightWay(dp: DockingPoints): (Boolean, Boolean) = {
    val vv1 = (dp.curP1 - dp.curP2).n * dock_dist
    val vv2 = vv1.perpendicular

    val p1_on_the_right_way = (curP1 - (dp.curP1 + vv1)).perpendicular * vv2 < 0 && (curP1 - (dp.curP1 - vv1)).perpendicular * vv2 > 0 // p1 inside line
    val p2_on_the_right_way = (curP2 - (dp.curP2 + vv1)).perpendicular * vv2 < 0 && (curP2 - (dp.curP2 - vv1)).perpendicular * vv2 > 0 // p2_inside_line
    (p1_on_the_right_way, p2_on_the_right_way)
  }
}
