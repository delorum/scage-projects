package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._

package object orbitalkiller {
  val G:Float = 20
  val base_dt = 0.01f // 1/60 секунды

  case class BodyState(index:String,
                       mass:Float,
                       I:Float,
                       force:Vec,
                       acc:Vec,
                       vel:Vec,
                       coord:Vec,
                       torque:Float,
                       ang_acc:Float,
                       ang_vel:Float,
                       ang:Float)

  def systemEvolutionFrom(dt: => Float,
                          force: (Long, BodyState, List[BodyState]) => Vec,
                          torque: (Long, BodyState, List[BodyState]) => Float)
                         (current_state:(Long, List[BodyState])):Stream[(Long, List[BodyState])] = {
    val (time, bodies) = current_state

    val next_time = time + (dt/base_dt).toLong
    val next_bodies = bodies.map { case bs =>
      val other_bodies = bodies.filterNot(_ == bs)
      val next_force = force(time, bs, other_bodies)
      val next_acc = next_force / bs.mass
      val next_vel = bs.vel + next_acc*dt
      val next_coord = bs.coord + next_vel*dt

      val next_torque = -torque(time, bs, other_bodies)
      val next_ang_acc = (next_torque / bs.I)/math.Pi.toFloat*180f  // in degrees
      val next_ang_vel = bs.ang_vel + next_ang_acc*dt
      val next_ang = (bs.ang + next_ang_vel*dt) % 360f

      bs.copy(force = next_force,
        acc = next_acc,
        vel = next_vel,
        coord = next_coord,
        torque = next_torque,
        ang_acc= next_ang_acc,
        ang_vel = next_ang_vel,
        ang = next_ang)
    }

    val pewpew = (next_time, next_bodies)
    pewpew #:: systemEvolutionFrom(dt, force, torque)(pewpew)
  }

  def gravityForce(body1_coord:Vec, body1_mass:Float, body2_coord:Vec, body2_mass:Float):Vec = {
    (body1_coord - body2_coord).n*G*body1_mass*body2_mass/body1_coord.dist2(body2_coord)
  }

  def satelliteSpeed(body_coord:Vec, planet_coord:Vec, planet_mass:Float):Vec = {
    val from_planet_to_body = body_coord - planet_coord
    from_planet_to_body.n.rotateDeg(90)*math.sqrt(G*planet_mass/from_planet_to_body.norma)
  }

  /**
   *
   * @param force - вектор силы
   * @param force_position - точка приложения силы
   * @param sin_angle - синус угла между вектором от центра масс до точки приложения силы и вектором силы
   * @return
   */
  def torque(force:Vec, force_position:Vec, sin_angle:Float):Float = force.norma*force_position.norma*sin_angle

  def torque(force:Vec, force_position:Vec, center:Vec):Float = {
    val x = force_position - center
    val xf = x*force.rotateDeg(90).n
    val sin_angle = xf/x.norma
    torque(force, force_position, sin_angle)
  }

  def maxOption[T](l:Seq[T])(implicit o:Ordering[T]):Option[T] = if(l.isEmpty) None else Some(l.max(o))
}
