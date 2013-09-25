package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scage.ScageLib.Vec
import net.phys2d.raw.collide.{Collider => Phys2dCollider, _}
import net.phys2d.raw.{Body => Phys2dBody, StaticBody => Phys2dStaticBody, BodyList => Phys2dBodyList, World => Phys2dWorld}
import net.phys2d.raw.shapes.{DynamicShape => Phys2dShape, _}
import scala.collection.mutable
import scala.Some

package object orbitalkiller {
  val G:Float = 20
  val base_dt = 0.01f // 1/60 секунды

  case class AABB(center:Vec, width:Float, height:Float) {
    val half_width = width/2
    val half_height = height/2

    def aabbCollision(b2:AABB):Boolean = {
      val d1 = math.abs(center.x - b2.center.x)
      (d1 < half_width + b2.half_width) && {
        val d2 = math.abs(center.y - b2.center.y)
        d2 < half_height + b2.half_height
      }
    }
  }

  def aabbCollision(b1:AABB, b2:AABB):Boolean = {
    val d1 = math.abs(b1.center.x - b2.center.x)
    (d1 < b1.half_width + b2.half_width) && {
      val d2 = math.abs(b1.center.y - b2.center.y)
      d2 < b1.half_height + b2.half_height
    }
  }

  sealed trait Shape {
    def aabb(center:Vec, rotation:Float):AABB
    def phys2dShape:Phys2dShape
    def wI:Float
  }

  case class CircleShape(radius:Float) extends Shape {
    def aabb(center:Vec, rotation:Float): AABB = {
      AABB(center, radius*2, radius*2)
    }

    def phys2dShape: Phys2dShape = new Circle(radius)

    lazy val wI: Float = radius*radius/2f
  }

  /*
  * Due to lack of line colliding algorithms, bodies with line shapes may be static only. If you want dynamic, use boxes
  * */
  case class LineShape(to:Vec) extends Shape {
    /*val center = from + (to - from)/2f

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

    def distanceSquared(point:Vec):Float = closestPoint(point).dist2(point)

    val vec = to - from
    def dx = vec.x
    def dy = vec.y*/

    def aabb(from:Vec, rotation:Float): AABB = {
      val to2 = from + to.rotateDeg(rotation)
      val center = from + (to2 - from)/2f
      AABB(center, math.max(math.abs(to2.x - from.x), 5f),  math.max(math.abs(to2.y - from.y), 5f))
    }

    def phys2dShape: Phys2dShape = new Line(to.x, to.y)

    lazy val wI: Float = to.norma2/12f
  }

  case class BoxShape(width:Float, height:Float) extends Shape {
    def aabb(center:Vec, rotation:Float): AABB = {
      val one = center + Vec(-width/2, height/2).rotateDeg(rotation)
      val two = center + Vec(width/2, height/2).rotateDeg(rotation)
      val three = center + Vec(width/2, -height/2).rotateDeg(rotation)
      val four = center + Vec(-width/2, -height/2).rotateDeg(rotation)
      val points = List(one, two, three, four)
      val xs = points.map(p => p.x)
      val ys = points.map(p => p.y)
      val min_x = xs.min
      val max_x = xs.max
      val min_y = ys.min
      val max_y = ys.max
      AABB(center, max_x - min_x, max_y - min_y)
    }

    def phys2dShape: Phys2dShape = new Box(width, height)

    lazy val wI: Float = (width*width + height*height)/12f
  }

  case class PolygonShape(points:List[Vec]) extends Shape {
    def aabb(center:Vec, rotation:Float): AABB = {
      val r = math.sqrt(points.map(p => p.norma2).max).toFloat*2
      AABB(center, r, r)
    }

    def phys2dShape: Phys2dShape = new Polygon(points.map(_.toPhys2dVec).toArray)

    lazy val wI: Float = {
      val numerator = (for {
        n <- 0 until points.length-2
        p_n_plus_1 = points(n+1)
        p_n = points(n)
      } yield p_n_plus_1.*/(p_n) * (p_n_plus_1.norma2 + (p_n_plus_1 * p_n) + p_n.norma2)).sum
      val denominator = (for {
        n <- 0 until points.length-2
        p_n_plus_1 = points(n+1)
        p_n = points(n)
      } yield p_n_plus_1.*/(p_n)).sum
      numerator/denominator/6f
    }
  }

  class Space(val bodies:List[BodyState], val center:Vec, val width:Float, val height:Float) {
    def this(bodies:List[BodyState], center:Vec) = {
      this(bodies, center, {
        val (init_min_x, init_max_x) = {
          bodies.headOption.map(b => {
            val AABB(c, w, _) = b.aabb
            (c.x - w/2, c.x+w/2)
          }).getOrElse((0f, 0f))
        }
        val (min_x, max_x) = bodies.foldLeft((init_min_x, init_max_x)) {
          case ((res_min_x, res_max_x), b) =>
            val AABB(c, w, _) = b.aabb
            val new_min_x = c.x - w/2
            val new_max_x = c.x + w/2
            (
              if(new_min_x < res_min_x) new_min_x else res_min_x,
              if(new_max_x > res_max_x) new_max_x else res_max_x
              )
        }
        math.max(math.abs(max_x - center.x)*2, math.abs(center.x - min_x)*2)
      }, {
        val (init_min_y, init_max_y) = {
          bodies.headOption.map(b => {
            val AABB(c, _, h) = b.aabb
            (c.y-h/2, c.y+h/2)
          }).getOrElse((0f, 0f))
        }
        val (min_y, max_y) = bodies.foldLeft(init_min_y, init_max_y) {
          case ((res_min_y, res_max_y), b) =>
            val AABB(c, _, h) = b.aabb
            val new_min_y = c.y - h/2
            val new_max_y = c.y + h/2
            (
              if(new_min_y < res_min_y) new_min_y else res_min_y,
              if(new_max_y > res_max_y) new_max_y else res_max_y
              )
        }
        math.max(math.abs(max_y - center.y)*2, math.abs(center.y - min_y)*2)
      })
    }


    lazy val aabb:AABB = AABB(center, width, height)

    lazy val quadSpaces:List[Space] = {
      val AABB(c, w, h) = aabb

      val c1 = c + Vec(-w/4, -h/4)
      val aabb1 = AABB(c1, w/2, h/2)
      val bodies1 = bodies.filter(b => b.aabb.aabbCollision(aabb1))

      val c2 = c + Vec(-w/4, h/4)
      val aabb2 = AABB(c2, w/2, h/2)
      val bodies2 = bodies.filter(b => b.aabb.aabbCollision(aabb2))

      val c3 = c + Vec(w/4, h/4)
      val aabb3 = AABB(c3, w/2, h/2)
      val bodies3 = bodies.filter(b => b.aabb.aabbCollision(aabb3))

      val c4 = c + Vec(w/4, -h/4)
      val aabb4 = AABB(c4, w/2, h/2)
      val bodies4 = bodies.filter(b => b.aabb.aabbCollision(aabb4))

      List(new Space(bodies1, c1, w/2, h/2), new Space(bodies2, c2, w/2, h/2), new Space(bodies3, c3, w/2, h/2), new Space(bodies4, c4, w/2, h/2))
    }
  }

  def splitSpace(space:Space, max_level:Int, target:Int, level:Int = 0, spaces:List[Space] = Nil):List[Space] = {
    if(space.bodies.length <= target) space :: spaces
    else if(level > max_level) space :: spaces
    else {
      space.quadSpaces.flatMap {
        case s => splitSpace(s, max_level, target, level+1, spaces)
      }
    }
  }

  case class GeometricContactData(contact_point:Vec, normal:Vec)
  case class Contact(body1:BodyState, body2:BodyState, contact_point:Vec, normal:Vec)

  private val contacts = Array.fill(10)(new net.phys2d.raw.Contact)
  private val circle_circle_collider = new CircleCircleCollider
  private val line_circle_collider = new LineCircleCollider
  private val circle_box_collider = CircleBoxCollider.createCircleBoxCollider()
  private val box_circle_collider = new BoxCircleCollider
  private val line_box_collider = new LineBoxCollider
  private val box_box_collider = new BoxBoxCollider
  private val line_polygon_collider = new LinePolygonCollider
  private val polygon_box_collider = new PolygonBoxCollider
  private val polygon_circle_collider = new PolygonCircleCollider
  private val polygon_polygon_collider = new PolygonPolygonCollider
  private val line_line_collider = new LineLineCollider

  def maybeCollision(body1:BodyState, body2:BodyState):Option[Contact] = {
    def collide(pb1:Phys2dBody, pb2:Phys2dBody, collider:Phys2dCollider):Option[GeometricContactData] = {
      val num_contacts = collider.collide(contacts, pb1, pb2)
      if(num_contacts == 0) None
      else {
        num_contacts match {
          case 1 =>
            val contact_point = contacts(0).getPosition.toVec
            val normal = contacts(0).getNormal.toVec
            Some(GeometricContactData(contact_point, normal))
          case 2 =>
            val contact_point = (contacts(0).getPosition.toVec + contacts(1).getPosition.toVec)/2
            val normal = contacts(0).getNormal.toVec
            Some(GeometricContactData(contact_point, normal))
          case _ => None
        }
      }
    }
    body1.shape match {
      case c1:CircleShape =>
        body2.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, circle_circle_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case l2:LineShape =>
            collide(body2.phys2dBody, body1.phys2dBody, line_circle_collider).map(gcd => Contact(body1, body2, gcd.contact_point, -gcd.normal))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, circle_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case p2:PolygonShape =>
            collide(body2.phys2dBody, body1.phys2dBody, polygon_circle_collider).map(gcd => Contact(body1, body2, gcd.contact_point, -gcd.normal))
          case _ => None
        }
      case l1:LineShape =>
        body2.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_circle_collider).map(gcd => Contact(body2, body1, gcd.contact_point, gcd.normal))
          case l2:LineShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_line_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case p2:PolygonShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_polygon_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case _ => None
        }
      case b1:BoxShape =>
        body2.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, box_circle_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case l2:LineShape =>
            collide(body2.phys2dBody, body1.phys2dBody, line_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, -gcd.normal))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, box_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case p2:PolygonShape =>
            collide(body2.phys2dBody, body1.phys2dBody, polygon_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, -gcd.normal))
          case _ => None
        }
      case p1:PolygonShape =>
        body2.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, polygon_circle_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case l2:LineShape =>
            collide(body2.phys2dBody, body1.phys2dBody, line_polygon_collider).map(gcd => Contact(body1, body2, gcd.contact_point, -gcd.normal))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, polygon_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case p2:PolygonShape =>
            collide(body1.phys2dBody, body2.phys2dBody, polygon_polygon_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal))
          case _ => None
        }
      case _ => None
    }
  }

  case class BodyState(index:String,
                       mass:Float,
                       acc:Vec,
                       vel:Vec,
                       coord:Vec,
                       ang_acc:Float,
                       ang_vel:Float,
                       ang:Float,
                       shape: Shape,
                       is_static:Boolean) {
    def phys2dBody:Phys2dBody = {
      if(is_static) {
        val b = new Phys2dStaticBody(index, shape.phys2dShape)
        b.setPosition(coord.x, coord.y)
        b.setRotation(ang/180f*math.Pi.toFloat)
        b.setUserData((index, shape))
        b
      } else {
        val b = new Phys2dBody(index, shape.phys2dShape, mass)
        b.setPosition(coord.x, coord.y)
        b.setRotation(ang/180f*math.Pi.toFloat)
        b.adjustVelocity(vel.toPhys2dVec)
        b.adjustAngularVelocity(ang_vel/180f*math.Pi.toFloat)
        b.setUserData((index, shape))
        b
      }
    }

    def aabb = shape.aabb(coord, ang)

    def I = mass*shape.wI
  }

  implicit class Phys2dBody2BodyState(pb:Phys2dBody) {
    def toBodyState:Option[BodyState] = {
      pb.getUserData match {
        case (index:String, shape:Shape) =>
          Some(BodyState(
            index = index,
            mass = pb.getMass,
            acc = Vec.zero,
            vel = Vec(pb.getVelocity.getX, pb.getVelocity.getY),
            coord =  Vec(pb.getPosition.getX, pb.getPosition.getY),
            ang_acc = 0f,
            ang_vel = pb.getAngularVelocity/math.Pi.toFloat*180f,
            ang = pb.getRotation/math.Pi.toFloat*180f,
            shape = shape,
            is_static = pb.isStatic
          ))
        case _ => None
      }
    }
  }

  implicit class Phys2dBodyList2List(pbl:Phys2dBodyList) {
    def toList:List[(Phys2dBody, BodyState)] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield (pb, bs)).toList
    }
    def toBodyStateList:List[BodyState] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield bs).toList
    }
  }

  def systemEvolutionFrom(dt: => Float,
                          base_dt:Float = 0.01f,
                          elasticity:Float, // elasticity or restitution: 0 - inelastic, 1 - perfectly elastic, (va2 - vb2) = -e*(va1 - vb1)
                          force: (Long, BodyState, List[BodyState]) => Vec = (time, body, other_bodies) => Vec.zero,
                          torque: (Long, BodyState, List[BodyState]) => Float = (time, body, other_bodies) => 0f,
                          changeFunction:(Long, List[BodyState]) => (Long, List[BodyState]) =  (time, bodies) => (time, bodies))
                         (current_state:(Long, List[BodyState])):Stream[(Long, List[BodyState])] = {
    val (time, bodies) = changeFunction(current_state._1, current_state._2)
    val next_time = time + (dt/base_dt).toLong

    val collision_data = mutable.HashMap[String, (Vec, Float)]()
    for {
      space <- splitSpace(new Space(bodies, Vec.zero), 5, 10)
      if space.bodies.length > 1
      (b1, idx) <- space.bodies.zipWithIndex.init
      b2 <- space.bodies.drop(idx+1)
      if !b1.is_static || !b2.is_static
      c @ Contact(_ ,_, contact_point, normal) <- maybeCollision(b1, b2)
    } {
      val rap = contact_point - b1.coord
      val n = normal.n
      val dv = b1.vel - b2.vel
      val relative_movement = dv*n
      if(relative_movement < 0) {  // If the objects are moving away from each other we dont need to apply an impulse
        collision_data += (b1.index -> (b1.vel, b1.ang_vel))
        collision_data += (b2.index -> (b2.vel, b2.ang_vel))
      } else {
        val ma = b1.mass
        val ia = b1.I

        val va1 = b1.vel
        val wa1 = b1.ang_vel/180f*math.Pi.toFloat // ang_vel in degrees, wa1 must be in radians
        val mb = b2.mass

        val e = elasticity
        if(mb == -1) {  // infinite mass
          val vap1 = va1 + (wa1 * rap.perpendicular)
          val j = -(1+e)*(vap1*n)/(1f/ma + (rap*/n)*(rap*/n)/ia)

          val va2 = va1 + (j * n)/ma
          val wa2 = (wa1 + (rap*/(j * n))/ia)/math.Pi.toFloat*180f  // must be in degrees

          collision_data += (b1.index -> (va2, wa2))
        } else {
          val ib = b2.I
          val rbp = contact_point - b2.coord
          val vb1 = b2.vel
          val wb1 = b2.ang_vel/180f*math.Pi.toFloat  // ang_vel in degrees, wb1 must be in radians
          val vab1 = va1 + (wa1 * rap.perpendicular) - vb1 - (wb1 * rbp.perpendicular)
          val j = -(1+e) * vab1*n/(1f/ma + 1f/mb + (rap*/n)*(rap*/n)/ia + (rbp*/n)*(rbp*/n)/ib)

          val va2 = va1 + (j * n)/ma
          val wa2 = (wa1 + (rap*/(j * n))/ia)/math.Pi.toFloat*180f  // must be in degrees
          collision_data += (b1.index -> (va2, wa2))

          val vb2 = vb1 - (j * n)/mb
          val wb2 = (wb1 - (rbp*/(j * n))/ib)/math.Pi.toFloat*180f  // must be in degrees
          collision_data += (b2.index -> (vb2, wb2))
        }
      }
    }

    val next_bodies = bodies.map { case b1 =>
      if(b1.is_static) b1
      else {
        val other_bodies = bodies.filterNot(_ == b1)

        val next_force = force(time, b1, other_bodies)
        val next_acc = next_force / b1.mass
        val next_vel = collision_data.get(b1.index).map(_._1).getOrElse(b1.vel + next_acc*dt)
        val next_coord = b1.coord + next_vel*dt

        val next_torque = torque(time, b1, other_bodies)
        val next_ang_acc = (next_torque / b1.I)/math.Pi.toFloat*180f  // in degrees
        val next_ang_vel = collision_data.get(b1.index).map(_._2).getOrElse(b1.ang_vel + next_ang_acc*dt)
        val next_ang = (b1.ang + next_ang_vel*dt) % 360f

        b1.copy(
          acc = next_acc,
          vel = next_vel,
          coord = next_coord,
          ang_acc= next_ang_acc,
          ang_vel = next_ang_vel,
          ang = next_ang
        )
      }
    }

    val pewpew = (next_time, next_bodies)
    pewpew #:: systemEvolutionFrom(dt, base_dt, elasticity, force, torque, changeFunction)(pewpew)
  }

  def gravityForce(body1_coord:Vec, body1_mass:Float, body2_coord:Vec, body2_mass:Float, G:Float):Vec = {
    (body1_coord - body2_coord).n*G*body1_mass*body2_mass/body1_coord.dist2(body2_coord)
  }

  def satelliteSpeed(body_coord:Vec, planet_coord:Vec, planet_mass:Float, G:Float):Vec = {
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
