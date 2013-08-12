package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._

package object orbitalkiller {
  val G:Float = 20
  val base_dt = 0.01f // 1/60 секунды

  case class M3(row1:(Float, Float, Float), row2:(Float, Float, Float), row3:(Float, Float, Float)) {
    val (a11, a12, a13) = row1
    val (a21, a22, a23) = row2
    val (a31, a32, a33) = row3

    val (a, b, c) = row1
    val (d, e, f) = row2
    val (g, h, k) = row3

    val rows = List(List(a11, a12, a13),
                    List(a21, a22, a23),
                    List(a31, a32, a33))

    val columns = List(List(a11, a21, a31),
                       List(a12, a22, a32),
                       List(a13, a23, a33))
    val det = {
      a11*a22*a33 - a11*a23*a32 - a12*a21*a33 + a12*a23*a31 + a13*a21*a32 - a13*a22*a31
    }

    val t = M3(
      (a11, a21, a31),
      (a12, a22, a32),
      (a13, a23, a33)
    )

    val inv:Option[M3] = {
      if(det == 0) None
      else {
        Some(M3(
          row1 = ((e*k - f*h)/det, -(b*k - c*h)/det, (b*f - c*e)/det),
          row2 = (-(d*k - f*g)/det, (a*k - c*g)/det, -(a*f - c*d)/det),
          row3 = ((d*h - e*g)/det, -(a*h - b*g)/det, (a*e - b*d)/det)
        ))
      }
    }

    def *(m:M3):M3 = {
      val p = for {
        (row, m_row) <- rows.zip(m.columns)
      } yield {
        for {
          (a, b) <- row.zip(m_row)
        } yield a*b
      }
      M3(
        row1 = (p(0)(0), p(0)(1), p(0)(2)),
        row2 = (p(1)(0), p(1)(1), p(1)(2)),
        row3 = (p(2)(0), p(2)(1), p(2)(2))
      )
    }

    def *(v:(Float, Float, Float)):(Float, Float, Float) = {
      val (v1, v2, v3) = v
      (a11*v1 + a12*v2 + a13*v3,
       a21*v1 + a22*v2 + a23*v3,
       a31*v1 + a32*v2 + a33*v3)
    }

    def *(x:Float):M3 = {
      M3(
        (a11*x, a21*x, a31*x),
        (a12*x, a22*x, a32*x),
        (a13*x, a23*x, a33*x)
      )
    }
  }

  case class AABB(center:Vec, width:Float, height:Float) {
    val half_width = width/2
    val half_height = height/2
  }

  def aabbCollision(b1:AABB, b2:AABB):Boolean = {
    val d1 = math.abs(b1.center.x - b2.center.x)
    d1 < b1.half_width + b2.half_width && {
      val d2 = math.abs(b1.center.y - b2.center.y)
      d2 < b1.half_height + b2.half_height
    }
  }

  sealed trait Shape

  case class CircleShape(center:Vec, radius:Float) extends Shape

  case class InfiniteLineShape(line_normal:Vec, distance_from_origin:Float) extends Shape

  case class FiniteLineShape(from:Vec, to:Vec) extends Shape {
    private val Vec(fx, fy) = from
    private val Vec(tx, ty) = to

    // y = a*x + b
    val a = (fy - ty)/(fx - tx)
    val b = (fx*ty - fy*tx)/(fx - tx)

    val line_normal:Vec = Vec(a, b).n
    val distance_from_origin:Float = -ty/math.sqrt(ty*ty + ((-b/a) - tx)*((-b/a) - tx)).toFloat*(b/a)
  }

  case class Contact(contact_point:Vec, penetration_distance:Float)

  def collision(s1:Shape, s2:Shape):Option[Contact] = {
    s1 match {
      case c1:CircleShape =>
        s2 match {
          case c2:CircleShape => circleCircleCollision(c1, c2)
          case _ => None
        }
      case _ => None
    }
  }

  def circleCircleCollision(c1:CircleShape, c2:CircleShape):Option[Contact] = {
    val d2 = c1.center.dist2(c2.center)
    if(d2 >= (c1.radius + c2.radius)*(c1.radius + c2.radius)) None
    else {
      val penetration_distance = (c1.radius + c2.radius - math.sqrt(d2).toFloat)/2
      val n = (c1.center - c2.center).n
      val contact_point = c1.center - n*(c1.radius - penetration_distance)
      Some(Contact(contact_point, penetration_distance))
    }
  }

  def circleInfiniteLineCollision(c:CircleShape, il:InfiniteLineShape):Option[Contact] = {
    val separation = il.line_normal*c.center + il.distance_from_origin
    if(separation >= c.radius) None
    else {
      val penetration_distance = c.radius - separation
      val contact_point = c.center - il.line_normal*penetration_distance
      Some(Contact(contact_point, penetration_distance))
    }
  }

  def circleFiniteLineCollision(c:CircleShape, il:FiniteLineShape):Option[Contact] = {
    val separation = il.line_normal*c.center + il.distance_from_origin
    if(separation >= c.radius) None
    else {
      val penetration_distance = c.radius -   separation
      val contact_point = c.center - il.line_normal*penetration_distance
      if(!areLinesIntersect(il.from, il.to, c.center, contact_point)) None
      else {
        Some(Contact(contact_point, penetration_distance))
      }
    }
  }

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
                       ang:Float,
                       elasticity:Float,
                       shape: (Vec, Float) => Shape)

  def systemEvolutionFrom(dt: => Float,
                          force: (Long, BodyState, List[BodyState]) => Vec,
                          torque: (Long, BodyState, List[BodyState]) => Float)
                         (current_state:(Long, List[BodyState])):Stream[(Long, List[BodyState])] = {
    val (time, bodies) = current_state

    val next_time = time + (dt/base_dt).toLong
    val next_bodies = bodies.map { case bs =>
      val other_bodies = bodies.filterNot(_ == bs)

      val collisions = other_bodies.flatMap {
        case ob =>
          val s1 = bs.shape(bs.coord, bs.ang)
          val s2 = ob.shape(ob.coord, ob.ang)
          val c = collision(s1, s2)
          if (c.nonEmpty) Some((c.get, ob)) else None
      }
      collisions.foldLeft(Vec.zero) {
        case (res_velocity, (Contact(contact_point, penetration_distance), ob)) =>
          val r0 = contact_point - bs.coord
          val r1 = contact_point - ob.coord
          val v0 = bs.vel + r0*bs.ang_vel
          val v1 = ob.vel + r1*ob.ang_vel
          val dv = v0 - v1
          val normal = r0.n

          Vec.zero
      }

      val next_force = force(time, bs, other_bodies)
      val next_acc = next_force / bs.mass
      val next_vel = bs.vel + next_acc*dt
      val next_coord = bs.coord + next_vel*dt

      val next_torque = torque(time, bs, other_bodies)
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
   * @param force_position_from_mass_center - точка приложения силы относительно центра масс
   * @param sin_angle - синус угла между вектором от центра масс до точки приложения силы и вектором силы
   * @return
   */
  def torque(force:Vec, force_position_from_mass_center:Vec, sin_angle:Float):Float = {
    force.norma*force_position_from_mass_center.norma*sin_angle
  }

  def torque(force:Vec, force_position_from_mass_center:Vec):Float = {
    val xf        = force_position_from_mass_center*force.rotateDeg(90).n
    val sin_angle = xf/force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def torque(force:Vec, force_position:Vec, center:Vec):Float = {
    val force_position_from_mass_center = force_position - center
    val xf                              = force_position_from_mass_center*force.rotateDeg(90).n
    val sin_angle                       = xf/force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def maxOption[T](l:Seq[T])(implicit o:Ordering[T]):Option[T] = if(l.isEmpty) None else Some(l.max(o))
}
