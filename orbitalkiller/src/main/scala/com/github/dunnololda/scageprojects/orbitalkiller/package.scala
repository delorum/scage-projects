package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{Body => Phys2dBody, BodyList => Phys2dBodyList, Collider => Phys2dCollider, DynamicShape => Phys2dShape, StaticBody => Phys2dStaticBody, _}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

package object orbitalkiller {
  val G:Double = 6.6742867E-11

  // axis-aligned bounding box
  case class AABB(center:DVec, width:Double, height:Double) {
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
    def aabb(center:DVec, rotation:Double):AABB
    def phys2dShape:Phys2dShape
    def wI:Double
  }

  case class CircleShape(radius:Double) extends Shape {
    def aabb(center:DVec, rotation:Double): AABB = {
      AABB(center, radius*2, radius*2)
    }

    def phys2dShape: Phys2dShape = new Circle(radius)

    lazy val wI: Double = radius*radius/2.0
  }

  /*
  * Due to lack of line colliding algorithms, bodies with line shapes may be static only. If you want dynamic, use boxes
  * */
  case class LineShape(to:DVec) extends Shape {
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

    def aabb(from:DVec, rotation:Double): AABB = {
      val to2 = from + to.rotateDeg(rotation)
      val center = from + (to2 - from)/2.0
      AABB(center, math.max(math.abs(to2.x - from.x), 5.0),  math.max(math.abs(to2.y - from.y), 5.0))
    }

    def phys2dShape: Phys2dShape = new Line(to.x, to.y)

    lazy val wI: Double = to.norma2/12.0
  }

  case class BoxShape(width:Double, height:Double) extends Shape {
    def aabb(center:DVec, rotation:Double): AABB = {
      val one = center + DVec(-width/2, height/2).rotateDeg(rotation)
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
      AABB(center, max_x - min_x, max_y - min_y)
    }

    def phys2dShape: Phys2dShape = new Box(width, height)

    lazy val wI: Double = (width*width + height*height)/12.0
  }

  implicit class DVec2DoublePhys2dVector(v:DVec) {
    def toPhys2dVecD = new Vector2f(v.x, v.y)
  }

  case class PolygonShape(points:List[DVec], convex_parts:List[PolygonShape]) extends Shape {
    def aabb(center:DVec, rotation:Double): AABB = {
      val r = math.sqrt(points.map(p => p.norma2).max)*2
      AABB(center, r, r)
    }

    def phys2dShape: Phys2dShape = new Polygon(points.map(_.toPhys2dVecD).toArray)

    lazy val wI: Double = {
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
      numerator/denominator/6.0
    }
  }

  class Space(val bodies:Seq[MutableBodyState], val center:DVec, val width:Double, val height:Double) {
    def this(bodies:Seq[MutableBodyState], center:DVec) = {
      this(bodies, center, {
        val (init_min_x, init_max_x) = {
          bodies.headOption.map(b => {
            val AABB(c, w, _) = b.aabb
            (c.x - w/2, c.x+w/2)
          }).getOrElse((0.0, 0.0))
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
          }).getOrElse((0.0, 0.0))
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

      val c1 = c + DVec(-w/4, -h/4)
      val aabb1 = AABB(c1, w/2, h/2)
      val bodies1 = bodies.filter(b => b.aabb.aabbCollision(aabb1))

      val c2 = c + DVec(-w/4, h/4)
      val aabb2 = AABB(c2, w/2, h/2)
      val bodies2 = bodies.filter(b => b.aabb.aabbCollision(aabb2))

      val c3 = c + DVec(w/4, h/4)
      val aabb3 = AABB(c3, w/2, h/2)
      val bodies3 = bodies.filter(b => b.aabb.aabbCollision(aabb3))

      val c4 = c + DVec(w/4, -h/4)
      val aabb4 = AABB(c4, w/2, h/2)
      val bodies4 = bodies.filter(b => b.aabb.aabbCollision(aabb4))

      List(
        new Space(bodies1, c1, w/2, h/2),
        new Space(bodies2, c2, w/2, h/2),
        new Space(bodies3, c3, w/2, h/2),
        new Space(bodies4, c4, w/2, h/2)
      )
    }
  }

  /**
   * Реализация http://en.wikipedia.org/wiki/Quadtree
   * @param space - начальное пространство, которое будем или не будем разделять
   * @param max_level - желаемое максимальное количество разбиений области на подобласти
   * @param target - желаемое максимальное количество объектов в одной области
   * @param level - текущий уровень разделения
   * @param spaces - список пространств, который отдадим в качестве результата
   * @return
   */
  def splitSpace(space:Space, max_level:Int, target:Int, level:Int = 0, spaces:List[Space] = Nil):List[Space] = {
    if(space.bodies.length <= target) space :: spaces
    else if(level > max_level) space :: spaces
    else {
      space.quadSpaces.flatMap {
        case s => splitSpace(s, max_level, target, level+1, spaces)
      }
    }
  }

  case class GeometricContactData(contact_point:DVec, normal:DVec, separation:Double)
  case class Contact(a:BodyState, b:BodyState, contact_point:DVec, normal:DVec, separation:Double)
  case class MutableContact(a:MutableBodyState, b:MutableBodyState, contact_point:DVec, normal:DVec, separation:Double) {
    def solveCollision(_dt:Double) {
      a.contacts += this
      b.contacts += this
      /*val a_prev_vel = a.vel
      val b_prev_vel = b.vel*/

      val e = /*1*//*0.9*/math.min(a.restitution, b.restitution)

      // radii from centers of masses to contact
      val ra = contact_point - a.coord
      val rb = contact_point - b.coord

      val rv = b.vel + (b.ang_vel.toRad */ rb) - a.vel - (a.ang_vel.toRad */ ra)  // Relative velocity

      val contactVel = rv*normal // Relative velocity along the normal
      if(contactVel <= 0) { // Do not resolve if velocities are separating
        val raCrossN = ra */ normal
        val rbCrossN = rb */ normal

        val invMassSum = a.invMass + b.invMass + (raCrossN*raCrossN)*a.invI + (rbCrossN*rbCrossN)*b.invI
        val  j = (-(1.0f + e) * contactVel)/invMassSum///contact_points.length
        val impulse = normal * j
        a.applyCollisionImpulse(-impulse, ra, _dt)
        b.applyCollisionImpulse(impulse, rb, _dt)

        val t = (rv + normal*(-rv*normal)).n
        val jt = (-(rv*t))/invMassSum///contact_points.length
        if(math.abs(jt) > 0.0001) {
          // Coulumb's law
          val tangentImpulse = if(math.abs(jt) < j*a.staticFriction) {
            t * jt
          } else {
            t*j*(-a.dynamicFriction)
          }
          a.applyCollisionImpulse(-tangentImpulse, ra, _dt)
          b.applyCollisionImpulse(tangentImpulse, rb, _dt)
        }
      }

      /*if(!a.body.is_static) {
        println(s"скорость ${a.body.index} до столкновения: ${a_prev_vel.norma}, скорость после столкновения: ${a.vel.norma}")
      }
      if(!b.body.is_static) {
        println(s"скорость ${b.body.index} до столкновения: ${b_prev_vel.norma}, скорость после столкновения: ${b.vel.norma}")
      }*/
    }
    
    def positionalCorrection() {
      if(separation > 0.005) {
        val correction = separation/(a.invMass + b.invMass)
        if(correction != 0) {
          println(s"correction: separation=$separation correction_${a.index}=${-a.invMass*correction} correction_${b.index}=${b.invMass*correction}")
        }
        if(!a.is_static) a.coord += normal*(-a.invMass*correction)
        if(!b.is_static) b.coord += normal*b.invMass*correction
      }
    }

    def toImmutableForA = Contact(a.toImmutableBodyState,
                                  b.toImmutableBodyState,
                                  contact_point,
                                  normal,
                                  separation)
    def toImmutableForB = Contact(b.toImmutableBodyState,
                                  a.toImmutableBodyState,
                                  contact_point,
                                  -normal,
                                  separation)
  }

  private val contacts = Array.fill(10)(new colliders.phys2d.Contact)
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

  def maybeCollisions(body1:MutableBodyState, body2:MutableBodyState):List[MutableContact] = {
    def collide(pb1:Phys2dBody, pb2:Phys2dBody, collider:Phys2dCollider):List[GeometricContactData] = {
      val num_contacts = collider.collide(contacts, pb1, pb2)
      if(num_contacts == 0) Nil
      else {
        num_contacts match {
          case 1 =>
            val contact_point = contacts(0).getPosition.toDVec
            val normal = contacts(0).getNormal.toDVec
            List(GeometricContactData(contact_point, normal, math.abs(contacts(0).getSeparation)))
          case 2 =>
            val contact_points = List(contacts(0).getPosition.toDVec, contacts(1).getPosition.toDVec)
            val contact_point = contact_points.sum/contact_points.length
            val normal = contacts(0).getNormal.toDVec
            val separation = math.max(math.abs(contacts(0).getSeparation), math.abs(contacts(1).getSeparation))
            List(GeometricContactData(contact_point, normal, separation))
          case _ => Nil
        }
      }
    }
    if(!body1.aabb.aabbCollision(body2.aabb)) Nil
    else {
      body1.shape match {
        case c1: CircleShape =>
          body2.shape match {
            case c2: CircleShape =>
              collide(body1.phys2dBody, body2.phys2dBody, circle_circle_collider).map(gcd => {
                MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
              })
            case l2: LineShape =>
              collide(body2.phys2dBody, body1.phys2dBody, line_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
            case b2: BoxShape =>
              collide(body1.phys2dBody, body2.phys2dBody, circle_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case p2: PolygonShape =>
              if (p2.convex_parts.isEmpty) {
                collide(body2.phys2dBody, body1.phys2dBody, polygon_circle_collider).map(gcd => {
                  MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                })
              } else {
                p2.convex_parts.flatMap(p => {
                  collide(body2.phys2dBodyWithShape(p), body1.phys2dBody, polygon_circle_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                  })
                })
              }
            case _ => Nil
          }
        case l1: LineShape =>
          body2.shape match {
            case c2: CircleShape =>
              collide(body1.phys2dBody, body2.phys2dBody, line_circle_collider).map(gcd => MutableContact(body2, body1, gcd.contact_point, gcd.normal, gcd.separation))
            case l2: LineShape =>
              collide(body1.phys2dBody, body2.phys2dBody, line_line_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case b2: BoxShape =>
              collide(body1.phys2dBody, body2.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case p2: PolygonShape =>
              if (p2.convex_parts.isEmpty) {
                collide(body1.phys2dBody, body2.phys2dBody, line_polygon_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
              } else {
                p2.convex_parts.flatMap(p => {
                  collide(body1.phys2dBody, body2.phys2dBodyWithShape(p), line_polygon_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                })
              }
            case _ => Nil
          }
        case b1: BoxShape =>
          body2.shape match {
            case c2: CircleShape =>
              collide(body1.phys2dBody, body2.phys2dBody, box_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case l2: LineShape =>
              collide(body2.phys2dBody, body1.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
            case b2: BoxShape =>
              collide(body1.phys2dBody, body2.phys2dBody, box_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case p2: PolygonShape =>
              if (p2.convex_parts.isEmpty) {
                collide(body2.phys2dBody, body1.phys2dBody, polygon_box_collider).map(gcd => {
                  MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                })
              } else {
                p2.convex_parts.flatMap(p => {
                  collide(body2.phys2dBodyWithShape(p), body1.phys2dBody, polygon_box_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                  })
                })
              }
            case _ => Nil
          }
        case p1: PolygonShape =>
          body2.shape match {
            case c2: CircleShape =>
              if (p1.convex_parts.isEmpty) {
                collide(body1.phys2dBody, body2.phys2dBody, polygon_circle_collider).map(gcd => {
                  MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                })
              } else {
                p1.convex_parts.flatMap(p => {
                  collide(body1.phys2dBodyWithShape(p), body2.phys2dBody, polygon_circle_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                })
              }
            case l2: LineShape =>
              if (p1.convex_parts.isEmpty) {
                collide(body2.phys2dBody, body1.phys2dBody, line_polygon_collider).map(gcd => {
                  MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                })
              } else {
                p1.convex_parts.flatMap(p => {
                  collide(body2.phys2dBody, body1.phys2dBodyWithShape(p), line_polygon_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                  })
                })
              }
            case b2: BoxShape =>
              if (p1.convex_parts.isEmpty) {
                collide(body1.phys2dBody, body2.phys2dBody, polygon_box_collider).map(gcd => {
                  MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                })
              } else {
                p1.convex_parts.flatMap(p => {
                  collide(body1.phys2dBodyWithShape(p), body2.phys2dBody, polygon_box_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                })
              }
            case p2: PolygonShape =>
              if (p1.convex_parts.isEmpty) {
                if (p2.convex_parts.isEmpty) {
                  collide(body1.phys2dBody, body2.phys2dBody, polygon_polygon_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                } else {
                  p2.convex_parts.flatMap(p_2 => {
                    collide(body1.phys2dBody, body2.phys2dBodyWithShape(p_2), polygon_polygon_collider).map(gcd => {
                      MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                    })
                  })
                }
              } else {
                p1.convex_parts.flatMap(p_1 => {
                  if (p2.convex_parts.isEmpty) {
                    collide(body1.phys2dBodyWithShape(p_1), body2.phys2dBody, polygon_polygon_collider).map(gcd => {
                      MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                    })
                  } else {
                    p2.convex_parts.flatMap(p_2 => {
                      collide(body1.phys2dBodyWithShape(p_1), body2.phys2dBodyWithShape(p_2), polygon_polygon_collider).map(gcd => {
                        MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                      })
                    })
                  }
                })
              }
            case _ => Nil
          }
        case _ => Nil
      }
    }
  }

  case class BodyState(index:String,
                       mass:Double,
                       acc:DVec = DVec.zero,
                       vel:DVec = DVec.zero,
                       coord:DVec,
                       ang_acc:Double = 0,
                       ang_vel:Double = 0,
                       ang:Double = 0,
                       shape: Shape,
                       is_static:Boolean = false,
                       restitution:Double = 0.2,  // elasticity or restitution: 0 - inelastic, 1 - perfectly elastic, (va2 - vb2) = -e*(va1 - vb1)
                       staticFriction:Double = 0.5,
                       dynamicFriction:Double = 0.3,
                       collisions:List[Contact] = Nil,
                       collisions_dacc:DVec = DVec.zero,
                       collisions_dvel:DVec = DVec.zero,
                       collisions_d_ang_acc:Double = 0.0,
                       collisions_d_ang_vel:Double = 0.0) {
    val aabb = shape.aabb(coord, ang)
    val I = mass*shape.wI
    val invMass = if(is_static || mass == 0) 0 else 1.0/mass
    val invI = if(is_static || I == 0) 0 else 1.0/I

    def toMutableBodyState = new MutableBodyState(this)
  }

  class MutableBodyState(body:BodyState) {
    val init_aabb = body.aabb
    val restitution = body.restitution
    val invMass = body.invMass
    val invI = body.invI
    val staticFriction = body.staticFriction
    val dynamicFriction = body.dynamicFriction
    val is_static = body.is_static
    val shape = body.shape
    val index = body.index
    var mass = body.mass
    val I = body.I

    // ===========================================

    var acc:DVec = DVec.zero
    var vel:DVec = body.vel
    var coord:DVec = body.coord

    var ang_acc:Double = 0.0
    var ang_vel:Double = body.ang_vel
    var ang:Double = body.ang

    var dacc:DVec = DVec.zero
    var dvel:DVec = DVec.zero
    var d_ang_acc:Double = 0.0
    var d_ang_vel:Double = 0.0

    val contacts = ArrayBuffer[MutableContact]()

    def init(): Unit = {
      acc = DVec.zero
      ang_acc = 0.0
      dacc = DVec.zero
      dvel = DVec.zero
      d_ang_acc = 0.0
      d_ang_vel = 0.0
      aabb = body.shape.aabb(coord, ang)
      contacts.clear()
    }

    var aabb = body.shape.aabb(coord, ang)

    def phys2dBody = {
      if(body.is_static) {
        val x = new Phys2dStaticBody(body.index, body.shape.phys2dShape)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.setUserData((body.index, body.shape))
        x
      } else {
        val x = new Phys2dBody(body.index, body.shape.phys2dShape, body.mass)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.adjustVelocity(vel.toPhys2dVecD)
        x.adjustAngularVelocity(ang_vel.toRad)
        x.setUserData((body.index, body.shape))
        x
      }
    }

    def phys2dBodyWithShape(shape:Shape) = {
      if(body.is_static) {
        val x = new Phys2dStaticBody(body.index, shape.phys2dShape)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.setUserData((body.index, shape))
        x
      } else {
        val x = new Phys2dBody(body.index, shape.phys2dShape, body.mass)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.adjustVelocity(vel.toPhys2dVecD)
        x.adjustAngularVelocity(ang_vel.toRad)
        x.setUserData((body.index, shape))
        x
      }
    }

    def applyCollisionImpulse(impulse:DVec, contactVector:DVec, dt:Double) {
      if(!body.is_static) {
        val x = impulse * body.invMass
        acc += x/dt
        dacc += x/dt
        vel += x
        dvel += x


        val y = (body.invI * (contactVector */ impulse)).toDeg
        ang_acc += y/dt
        d_ang_acc += y/dt
        ang_vel += y
        d_ang_vel += y
      }
    }
    
    def toImmutableBodyState:BodyState = body.copy(
      coord = coord,
      acc = acc,
      vel = vel,
      ang_acc = ang_acc,
      ang_vel = ang_vel, 
      ang = ang,
      collisions_dacc = dacc,
      collisions_dvel = dvel,
      collisions_d_ang_acc = d_ang_acc,
      collisions_d_ang_vel = d_ang_vel)

    def copy:MutableBodyState = {
      val mb = new MutableBodyState(body)
      mb.mass = mass
      mb.acc = acc
      mb.vel = vel
      mb.coord = coord
      mb.ang_acc = ang_acc
      mb.ang_vel = ang_vel
      mb.ang = ang
      mb.dacc = dacc
      mb.dvel = dvel
      mb.d_ang_acc = d_ang_acc
      mb.d_ang_vel = d_ang_vel
      mb
    }

    override def toString = s"MutableBodyState($index)"
  }

  implicit class Phys2dBody2BodyState(pb:Phys2dBody) {
    def toBodyState:Option[BodyState] = {
      pb.getUserData match {
        case (index:String, shape:Shape) =>
          Some(BodyState(
            index = index,
            mass = pb.getMass,
            acc = DVec.dzero,
            vel = DVec(pb.getVelocity.getX, pb.getVelocity.getY),
            coord =  DVec(pb.getPosition.getX, pb.getPosition.getY),
            ang_acc = 0.0,
            ang_vel = pb.getAngularVelocity.toDeg,
            ang = pb.getRotation.toDeg,
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

  def correctAngle(angle:Double):Double = {
    if(angle > 360) correctAngle(angle - 360)
    else if(angle < 0) correctAngle(angle + 360)
    else angle
  }

  // структура хранит и по необходимости довычисляет набор простых чисел. Вычисление производится методом решета Эратосфена
  object erat2 {
    private var eratl = (2L, Seq[Long](2))
    def apply(n:Long):Seq[Long] = {
      def _erat(_n:Long, l:Seq[Long], p:Long):Seq[Long] = {
        println(p)
        if(p*p > _n) l
        else {
          val m = l.filterNot(x => x > p && x % p == 0)
          _erat(_n, m, m.find(_ > p).get)
        }
      }

      if(n > eratl._1) {
        println("generating primes...")
        val new_eratl_2 = _erat(n*2, eratl._2 ++ (eratl._1+1 to n*2), 2)
        eratl = (n*2, new_eratl_2)
      }
      eratl._2.view.takeWhile(_ <= n)
    }

    def clear() {
      eratl = (2L, Seq[Long](2))
      println("erased primes")
    }
  }

  // быстрая функция проверки, является ли число простым. Идея в том, чтобы проверить, делиться ли число n на простые от 2 до 
  // корня из числа n. Список простых формируется по мере надобности.
  def isPrime(n:Long, sqrt_n:Long = -1l, cur_p:Long = 1l):Boolean = {
    if(sqrt_n == -1l) isPrime(n, math.sqrt(n).toLong, cur_p)
    else {
      if(cur_p >= sqrt_n) true
      else {
        val new_primes_portion = erat2(math.min(cur_p*2, sqrt_n)).dropWhile(_ <= cur_p)
        if(new_primes_portion.isEmpty) true
        else {
          val res = new_primes_portion.forall(n % _ != 0)
          if(res) isPrime(n, sqrt_n, new_primes_portion.last)
          else false
        }
      }
    }
  }
  val pfacts = mutable.HashMap[Long, List[Long]]()

  // возвращает список простых чисел, произведение которых равно данному числу n
  def primeFactors3(n:Long):List[Long] = {
    def _primeFactors3(_n:Long, facs:List[Long] = Nil):List[Long] = {
      pfacts.get(_n).map(x => x ::: facs).getOrElse(
        erat2(math.sqrt(_n).toLong).find(_n % _ == 0) match {
          case Some(m) => _primeFactors3(_n/m, m :: facs)
          case None => _n :: facs
        }
      )
    }
    _primeFactors3(n)
  }

  // возвращает список простых чисел, произведение которых равно данному числу n, кеширует результат для ускорения последующих запросов
  def primeFactors4(n:Long) = {
    pfacts.getOrElseUpdate(n, primeFactors3(n))
  }

  // возвращает список делителей данного числа n (простых и непростых)
  def factors4(n:Long) = {
    val res23 = primeFactors4(n)
    (1L :: (1 to res23.length).flatMap(i => res23.combinations(i).map(_.product)).toList).filterNot(_ == n) ::: n :: Nil
  }

  private val facts = mutable.HashMap[Long, List[Long]]()

  // возвращает список делителей, кеширует результат
  def factors5(n:Long) = {
    facts.getOrElseUpdate(n, factors4(n))
  }

  case class CollisionData(
    collided_body:BodyState,
    contact_point:DVec,
    normal:DVec,
    separation:Double,
    contacts:List[net.phys2d.raw.Contact], new_velocity:DVec, new_angular_velocity:Double)

  def mutableSystemEvolution(mutable_system:Seq[(MutableBodyState, Seq[MutableBodyState])],
                             base_dt:Double,          // в секундах
                             force: (MutableBodyState, Seq[MutableBodyState]) => DVec = (body, other_bodies) => DVec.dzero,
                             torque: (MutableBodyState, Seq[MutableBodyState]) => Double = (body, other_bodies) => 0.0,
                             enable_collisions:Boolean = true,
                             system_center:DVec = DVec.zero): Unit = {
    mutable_system.foreach(_._1.init())

    val collisions = if(enable_collisions) {
      val x = splitSpace(new Space(mutable_system.map(_._1), system_center), 5, 2)
      for {
        space <- x
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx+1)
        if !b1.is_static || !b2.is_static
        c <- maybeCollisions(b1, b2)
      } yield c
    } else Nil

    // Integrate forces first part
    mutable_system.foreach {case (mb, other_mutable_bodies) =>
      if(!mb.is_static) {
        val next_force = force(mb, other_mutable_bodies)
        val next_acc = next_force * mb.invMass
        val next_torque = torque(mb, other_mutable_bodies)
        val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
        mb.acc = next_acc
        mb.vel += next_acc * base_dt * 0.5
        mb.ang_acc = next_ang_acc
        mb.ang_vel += next_ang_acc * base_dt * 0.5
      }
    }

    // Solve collisions
    if(collisions.nonEmpty) collisions.foreach(c => c.solveCollision(base_dt))

    // Integrate velocities and forces last part
    mutable_system.foreach{ case (mb2, other_mutable_bodies2) =>
      if(!mb2.is_static) {
        mb2.coord += mb2.vel*base_dt
        mb2.ang = correctAngle((mb2.ang + mb2.ang_vel*base_dt) % 360)
        val next_force2 = force(mb2, other_mutable_bodies2)
        val next_acc2 = next_force2 * mb2.invMass
        val next_torque2 = torque(mb2, other_mutable_bodies2)
        val next_ang_acc2 = (next_torque2 * mb2.invI).toDeg // in degrees
        mb2.acc += next_acc2
        mb2.vel += next_acc2 * base_dt * 0.5
        mb2.ang_acc += next_ang_acc2
        mb2.ang_vel += next_ang_acc2 * base_dt * 0.5
      }
    }

    // Correct positions
    if(collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())
  }

  def systemEvolutionFrom(dt: => Double,           // в секундах, может быть больше либо равно base_dt - обеспечивается ускорение времени
                          maxMultiplier: => Int = 1,
                          base_dt:Double,          // в секундах
                          force: (Long, BodyState, List[BodyState]) => DVec = (time, body, other_bodies) => DVec.dzero,
                          torque: (Long, BodyState, List[BodyState]) => Double = (time, body, other_bodies) => 0.0,
                          changeFunction:(Long, List[BodyState]) => (Long, List[BodyState]) =  (time, bodies) => (time, bodies),
                          enable_collisions:Boolean = true)
                         (current_state:(Long, List[BodyState])):Stream[(Long, List[BodyState])] = {
    def _step(state:(Long, List[BodyState]), _dt:Double, steps:Int):(Long, List[BodyState]) = {
      val (tacts, bodies) = changeFunction(state._1, state._2)
      val next_tacts = tacts + steps

      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-the-basics-and-impulse-resolution--gamedev-6331
      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-friction-scene-and-jump-table--gamedev-7756
      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-oriented-rigid-bodies--gamedev-8032
      // http://www.niksula.hut.fi/~hkankaan/Homepages/gravity.html

      val mutable_bodies = bodies.map(_.toMutableBodyState)

      val collisions = if(enable_collisions && _dt == base_dt) {for {
        space <- splitSpace(new Space(mutable_bodies, DVec.zero), 5, 3)
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx+1)
        if !b1.is_static || !b2.is_static
        c <- maybeCollisions(b1, b2)
      } yield c} else Nil
      
      val mb_and_others = for {
        (mb, idx) <- mutable_bodies.zipWithIndex
        other_mutable_bodies = mutable_bodies.take(idx) ::: mutable_bodies.drop(idx + 1)
      } yield (mb, other_mutable_bodies)
      
      // Integrate forces first part
      mb_and_others.foreach {case (mb, other_mutable_bodies) =>
        if(!mb.is_static) {
          val b = mb.toImmutableBodyState
          val other_bodies = other_mutable_bodies.map(_.toImmutableBodyState)
          val next_force = force(tacts, b, other_bodies)
          val next_acc = next_force * mb.invMass
          val next_torque = torque(tacts, b, other_bodies)
          val next_ang_acc = (next_torque * mb.invI).toDeg // in degrees
          mb.acc = next_acc
          mb.vel += next_acc * _dt * 0.5
          mb.ang_acc = next_ang_acc
          mb.ang_vel += next_ang_acc * _dt * 0.5
        }
      }

      // Solve collisions
      if(collisions.nonEmpty) collisions.foreach(c => c.solveCollision(_dt))

      // Integrate velocities and forces last part
      mb_and_others.foreach{ case (mb2, other_mutable_bodies2) =>
        if(!mb2.is_static) {
          val b2 = mb2.toImmutableBodyState
          mb2.coord += mb2.vel*_dt
          mb2.ang = correctAngle((mb2.ang + mb2.ang_vel*_dt) % 360)
          val other_bodies2 = other_mutable_bodies2.map(_.toImmutableBodyState)
          val next_force2 = force(tacts, b2, other_bodies2)
          val next_acc2 = next_force2 * b2.invMass
          val next_torque2 = torque(tacts, b2, other_bodies2)
          val next_ang_acc2 = (next_torque2 * b2.invI).toDeg // in degrees
          mb2.acc += next_acc2
          mb2.vel += next_acc2 * _dt * 0.5
          mb2.ang_acc += next_ang_acc2
          mb2.ang_vel += next_ang_acc2 * _dt * 0.5
        }
      }
      
      // Correct positions
      if(collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

      val x = collisions.groupBy(mc => mc.a.index).map(kv => (kv._1, kv._2.map(_.toImmutableForA)))
      val y = collisions.groupBy(mc => mc.b.index).map(kv => (kv._1, kv._2.map(_.toImmutableForB)))
      val z = (x.keySet ++ y.keySet).map(body_index => (body_index, x.getOrElse(body_index, Nil) ::: y.getOrElse(body_index, Nil))).toMap

      val next_bodies = mutable_bodies.map(mb => mb.toImmutableBodyState.copy(collisions = z.getOrElse(mb.index, Nil)))

      (next_tacts, next_bodies)
    }
    val cur_dt = dt
    val max_multiplier = maxMultiplier
    val steps = math.max((cur_dt/base_dt).toInt, 1)

    val next_state = if(steps < max_multiplier) {
      _step(current_state, cur_dt, steps)
    } else {
      if(max_multiplier == 1) {
        (1 to steps).foldLeft(current_state) {
          case (state, step) =>
            _step(state, base_dt, 1)
        }
      } else {
        // пусть maxMultiplier = 450, а мы хотим ускорение 1000
        // тогда подбираем ближайший multiplier меньше 450, на который делится 1000 без остатка
        val multiplier = factors5(steps).filter(_ <= max_multiplier).max.toInt
        val steps2 = steps/multiplier
        (1 to steps2).foldLeft(current_state) {
          case (state, step) =>
            _step(state, base_dt*multiplier, multiplier)
        }
      }
    }

    next_state #:: systemEvolutionFrom(dt, maxMultiplier, base_dt, force, torque, changeFunction, enable_collisions)(next_state)
  }

  def gravityForce(body1_coord:DVec, body1_mass:Double, body2_coord:DVec, body2_mass:Double, G:Double):DVec = {
    (body1_coord - body2_coord).n*G*body1_mass*body2_mass/body1_coord.dist2(body2_coord)
  }

  private def _satelliteSpeed(from_planet_to_body:DVec, planet_velocity:DVec, planet_mass:Double, G:Double, counterclockwise:Boolean):DVec = {
    val sat_speed = from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)
    if(!counterclockwise) planet_velocity + sat_speed*(-1)
    else planet_velocity + sat_speed
  }

  def satelliteSpeed(body_coord:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double, counterclockwise:Boolean):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def satelliteSpeed(body_coord:DVec, body_velocity:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular*(body_velocity - planet_velocity) >= 0
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  private def _escapeVelocity(from_planet_to_body:DVec, planet_velocity:DVec, planet_mass:Double, G:Double, counterclockwise:Boolean):DVec = {
    val esc_speed = from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*math.sqrt(2)
    if(!counterclockwise) planet_velocity + esc_speed*(-1)
    else planet_velocity + esc_speed
  }

  def escapeVelocity(body_coord:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double, counterclockwise:Boolean):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def escapeVelocity(body_coord:DVec, body_velocity:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular*(body_velocity - planet_velocity) >= 0
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def timeStr(time_msec:Long,add_plus_sign:Boolean = false):String = {
    val is_below_zero = time_msec < 0
    val abs_time_msec = math.abs(time_msec)
    val result = if (abs_time_msec < 1000) s"$abs_time_msec мсек."
    else {
      val sec  = 1000l
      val min  = sec*60
      val hour = min*60
      val day  = hour*24

      List(
        (abs_time_msec/day,      "д."),
        (abs_time_msec%day/hour, "ч."),
        (abs_time_msec%hour/min, "мин."),
        (abs_time_msec%min/sec,  "сек."),
        (abs_time_msec%sec,      "мсек.")
      ).filter(_._1 > 0).map(e => e._1+" "+e._2).mkString(" ")
    }
    if(is_below_zero) s"-$result" else {
      if(add_plus_sign) s"+$result"
      else result
    }
  }

  def mOrKmOrMKm(meters:Number):String = {
    val d = meters.doubleValue()
    val absd = math.abs(d)
    if(absd < 1000) {
      f"$d%.2f м"
    } else if(absd < 1000000) {
      f"${d/1000}%.2f км"
    } else if(absd < 1000000000) {
      f"${d/1000000}%.2f тыс. км"
    } else {
      f"${d/1000000000}%.2f млн. км"
    }
  }

  def msecOrKmsec(msec:Number):String = {
    if(math.abs(msec.doubleValue()) < 1000) f"${msec.doubleValue()}%.2f м/сек" else f"${msec.doubleValue()/1000}%.2f км/сек"
  }

  def newtonOrKilonewton(newton:Number):String = {
    if(math.abs(newton.doubleValue()) < 1000) f"${newton.doubleValue()}%.2f Н" else f"${newton.doubleValue()/1000}%.2f кН"
  }

  /**
   * Ускорение
   * @param msec - метры в секунду
   * @return
   */
  def msec2OrKmsec2(msec:Number):String = {
    if(math.abs(msec.doubleValue()) < 1000) f"${msec.doubleValue()}%.2f м/сек^2" else f"${msec.doubleValue()/1000}%.2f км/сек^2"
  }
  
  sealed trait KeplerOrbit {
    def a:Double
    def b:Double
    def e:Double
    def f:DVec
    def center:DVec
    def strDefinition(prefix:String, planet_radius:Double, planet_velocity:DVec, planet_g:Double, ship_coord:DVec, ship_velocity:DVec):String
  }

  class EllipseOrbit(
    val a:Double,             // большая полуось
    val b:Double,             // малая полуось
    val e:Double,             // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
    val c:Double,             // фокальное расстояния (полурасстояние между фокусами)
    val p:Double,             // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)
    val r_p:Double,           // перигей
    val r_a:Double,           // апогей
    val t:Double,             // орбитальный период, в секундах
    val f:DVec,               // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
    val f2:DVec,              // координаты второго фокуса
    val center:DVec,          // координаты центра
    mu:Double) extends KeplerOrbit {  // гравитационный параметр: произведение гравитационной постоянной G на сумму масс притягивающего центра и корабля на орбите
    def strDefinition(prefix:String, planet_radius:Double, planet_velocity:DVec, planet_g:Double, ship_coord:DVec, ship_velocity:DVec):String = {
      val ccw = (ship_coord - f).perpendicular*(ship_velocity - planet_velocity) >= 0   // летим против часовой?
      val dir = if(ccw) {
        "\u21b6"  // против часовой стрелки
      } else {
        "\u21b7"  // по часовой стрелке
      }
      if(r_p - planet_radius < 0) {
        val y_axis = (ship_coord - f).n
        //val y0 = (ship_coord - f)*y_axis - planet_radius
        val v0y = (ship_velocity - planet_velocity)*y_axis
        //val fall_time_sec = (v0y + math.sqrt(2*planet_g*y0 + v0y*v0y))/planet_g
        /*val fall_time_sec = {
          // https://www.rand.org/content/dam/rand/pubs/research_memoranda/2008/RM3752.pdf
          // page 6-7
          val r_L = ship_coord.dist(f)
          val r_T = planet_radius + 3.5
          val V_sL = math.sqrt(planet_g*planet_radius*planet_radius/r_L)
          val V_L = (ship_velocity - planet_velocity).norma
          val Y_L = V_L/V_sL
          val gamma_L = (ship_velocity - planet_velocity).rad(y_axis.perpendicular)
          val t_LA = r_L/(V_sL*math.pow(2 - Y_L*Y_L, 3.0/2.0))*(math.acos((1 - Y_L*Y_L)/e) + Y_L*math.sin(gamma_L*math.sqrt(2 - Y_L*Y_L)))
          val t_AT = r_L/(V_sL*math.pow(2 - Y_L*Y_L, 3.0/2.0))*(math.acos(((1 - Y_L*Y_L) + (1 - r_L/r_T))/(r_L/r_T*e)) + math.sqrt(2 - Y_L*Y_L)/(r_L/r_T)*math.sqrt(2*(r_L/r_T - 1) + Y_L*Y_L*(1 - math.pow(r_L/r_T, 2)*math.pow(math.cos(gamma_L), 2))))
          val launch_before_apogee = v0y >= 0
          if(launch_before_apogee) t_LA + t_AT else -t_LA + t_AT
        }*/

        val fall_time_msec = if(ccw) {
          val fall_teta_rad = -math.acos((p/(planet_radius+3) - 1)/e) + 2*math.Pi
          travelTimeOnOrbitMsecCCW(ship_coord, orbitalPointByTrueAnomalyRad(fall_teta_rad))
        } else {
          val fall_teta_rad = math.acos((p/(planet_radius+3) - 1)/e)
          travelTimeOnOrbitMsecCW(ship_coord, orbitalPointByTrueAnomalyRad(fall_teta_rad))
        }

        val time_to_stop_at_full_power = math.abs(v0y/(1000000/OrbitalKiller.ship.mass - planet_g))
        val fall_time_str = if(fall_time_msec < 30000) s"[r Поверхность через ${timeStr(fall_time_msec)} (${timeStr((time_to_stop_at_full_power*1000l).toLong)})]" else s"Поверхность через ${timeStr(fall_time_msec)}"

        f"$prefix, суборбитальная, $dir, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, r_a = ${mOrKmOrMKm(r_a - planet_radius)}. $fall_time_str"
      } else {
        f"$prefix, замкнутая, $dir, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, r_a = ${mOrKmOrMKm(r_a - planet_radius)}, t = ${timeStr((t*1000l).toLong)}"
      }
    }

    private val f_minus_f2 = f - f2
    private val inv_n = a*math.sqrt(a/mu) // это 1/n

    def tetaDeg360ByDir(dir:DVec) = f_minus_f2.deg360(dir)
    def tetaSignedDegByDir(dir:DVec) = f_minus_f2.signedDeg(dir)
    def tetaRad2PiByDir(dir:DVec) = f_minus_f2.rad2Pi(dir)
    def tetaSignedRadByDir(dir:DVec) = f_minus_f2.signedRad(dir)

    def tetaDeg360InPoint(p:DVec) = tetaDeg360ByDir(p - f)
    def tetaSignedDegInPoint(p:DVec) = tetaSignedDegByDir(p - f)
    def tetaRad2PiInPoint(p:DVec) = tetaRad2PiByDir(p - f)
    def tetaSignedRadInPoint(p:DVec) = tetaSignedRadByDir(p - f)

    def tetaRadByDistance(r:Double):Double = {
      math.acos((p/r - 1)/e)
    }
    def tetaDegByDistance(r:Double):Double = {
      tetaRadByDistance(r)/math.Pi*180.0
    }

    def distanceByTrueAnomalyRad(teta_rad:Double) = {
      p/(1 + e*math.cos(teta_rad))
    }

    def distanceByTrueAnomalyDeg(teta_deg:Double) = {
      p/(1 + e*math.cos(teta_deg/180.0*math.Pi))
    }

    /**
     * Расстояние от притягивающего центра до орбиты в данном направлении. Определяем истинную аномалию данного направления
     * и по формуле считаем длину радиус-вектора.
     * https://en.wikipedia.org/wiki/True_anomaly#Radius_from_true_anomaly
     * @param dir - вектор направления
     * @return
     */
    def distanceByDir(dir:DVec) = {
      p/(1 + e*math.cos(tetaSignedRadByDir(dir)))
    }

    def distanceInPoint(point:DVec) = {
      p/(1 + e*math.cos(tetaSignedRadInPoint(point)))
    }

    def orbitalPointByTrueAnomalyRad(teta_rad:Double) = {
      f + f_minus_f2.rotateRad(teta_rad).n*distanceByTrueAnomalyRad(teta_rad)
    }

    def orbitalPointByTrueAnomalyDeg(teta_deg:Double) = {
      f + f_minus_f2.rotateDeg(teta_deg).n*distanceByTrueAnomalyDeg(teta_deg)
    }

    def orbitalPointByDir(dir:DVec) = {
      f + dir.n*distanceByDir(dir)
    }

    def orbitalPointInPoint(point:DVec) = {
      val dir = point - f
      f + dir.n*distanceByDir(dir)
    }

    private def _travelTimeOnOrbitMsecCCW(t1:Double, orbital_point1:DVec, t2:Double, orbital_point2:DVec):Long = {
      val r1 = (orbital_point1 - f).norma
      val r2 = (orbital_point2 - f).norma
      val s = orbital_point1.dist(orbital_point2)
      val xl1 = math.acos(1 - (r1+r2+s)/(2*a))
      val xl2 = math.acos(1 - (r1+r2-s)/(2*a))
      // Балк М.Б. Элементы динамики космического полета, Формула Ламберта, стр 128-129: выбор чисел l1, l2 среди корней уравнения
      // для эллиптической орбиты, анализ проведен английским математиком А. Кэли
      val (l1, l2, /*variant*/_) = if(t1 == 0) {
        if(t2 < 180) (xl1, xl2, "None")
        else (2*math.Pi - xl1, -xl2, "F & A")
      } else {
        if (areLinesIntersect(f2 + (f2 - f).n*r_p, f2, orbital_point1, orbital_point2)) {
          if (t2 > t1) (xl1, xl2, "None")
          else (2 * math.Pi - xl1, -xl2, "F & A")
        } else if (areLinesIntersect(f, f + (f - f2).n*r_p, orbital_point1, orbital_point2)) {
          if (t1 > t2) (xl1, xl2, "None")
          else (2 * math.Pi - xl1, -xl2, "F & A")
        } else if (areLinesIntersect(f2, f, orbital_point1, orbital_point2)) {
          if (t2 > t1) (2 * math.Pi - xl1, xl2, "F")
          else (xl1, -xl2, "A")
        } else {
          if (t2 > t1) (xl1, xl2, "None")
          else (2 * math.Pi - xl1, -xl2, "F & A")
        }
      }
      /*if(print_variant) {
        println(variant)
        /*if(variant == "F & A") {
          drawLine(orbital_point1*OrbitalKiller.scale, orbital_point2*OrbitalKiller.scale, YELLOW)
        }*/
      }*/
      // Балк М.Б. Элементы динамики космического полета, Формула Ламберта
      (inv_n*((l1 - math.sin(l1)) - (l2 - math.sin(l2)))).toLong*1000
    }

    /**
     * Время в миллисекундах, которое займет перемещение корабля по эллиптической орбите из точки point1 в точку point2 против часово стрелки.
     * Вычисляется по формуле Ламберта, которую нашел в книге М.Б. Балка "Элементы динамики космического полета", стр 122-129
     * http://pskgu.ru/ebooks/astro3/astro3_03_05.pdf
     * @param point1 - начальная точка
     * @param point2 - конечная точка
     * @return
     */
    def travelTimeOnOrbitMsecCCW(point1:DVec, point2:DVec, recalculate_orbital_points:Boolean = false/*, print_variant:Boolean = false*/):Long = {
      val t1 = tetaDeg360InPoint(point1)
      val t2 = tetaDeg360InPoint(point2)
      if(recalculate_orbital_points) {
        val orbital_point1 = orbitalPointByTrueAnomalyDeg(t1)
        val orbital_point2 = orbitalPointByTrueAnomalyDeg(t2)
        _travelTimeOnOrbitMsecCCW(t1, orbital_point1, t2, orbital_point2)
      } else {
        _travelTimeOnOrbitMsecCCW(t1, point1, t2, point2)
      }
    }

    def travelTimeOnOrbitMsecCCW(teta1Deg360:Double, teta2Deg360:Double/*, print_variant:Boolean*/):Long = {
      val orbital_point1 = orbitalPointByTrueAnomalyDeg(teta1Deg360)
      val orbital_point2 = orbitalPointByTrueAnomalyDeg(teta2Deg360)
      _travelTimeOnOrbitMsecCCW(teta1Deg360, orbital_point1, teta2Deg360, orbital_point2)
    }

    def travelTimeOnOrbitMsecCW(point1:DVec, point2:DVec):Long = {
      travelTimeOnOrbitMsecCCW(point2, point1)
    }

    def travelTimeOnOrbitMsecCW(teta1Deg360:Double, teta2Deg360:Double/*, print_variant:Boolean*/):Long = {
      val orbital_point1 = orbitalPointByTrueAnomalyDeg(teta1Deg360)
      val orbital_point2 = orbitalPointByTrueAnomalyDeg(teta2Deg360)
      _travelTimeOnOrbitMsecCCW(teta2Deg360, orbital_point2, teta1Deg360, orbital_point1)
    }

    /**
     * Орбитальная скорость в точке орбиты с данной истинной аномалией. Это скорость относительно притягивающего центра.
     * То есть, если планета в свою очередь движется по орбите, то чтобы получить абсолютную скорость, надо вычислять и прибавлять
     * эту скорость планеты в данный момент времени и так далее.
     * @param teta_rad - угол в радианах между радиус вектором на точку на орбите и направлением на перицентр.
     *                   Если против часовой стрелки - положительный, от 0 до pi, иначе отрицательный, от 0 до -pi
     *                   https://en.wikipedia.org/wiki/Lambert%27s_problem
     *                   https://en.wikipedia.org/wiki/Kepler_orbit
     * @return vr - радиальная компонента, направлена к притягивающему центру
     *         vt - перпендикулярна радиальной компоненте
     *         math.sqrt(vr*vr + vt*vt) даст числовое выражение скорости
     */
    def orbitalVelocityByTrueAnomalyRad(teta_rad:Double) = {
      val vr = math.sqrt(mu/p)*e*math.sin(teta_rad)
      val vt = math.sqrt(mu/p)*(1 + e*math.cos(teta_rad))
      (vt, vr)
    }

    def orbitalVelocityByDir(dir:DVec) = {
      orbitalVelocityByTrueAnomalyRad(tetaRad2PiByDir(dir))
    }

    def orbitalVelocityInPoint(point:DVec) = {
      orbitalVelocityByTrueAnomalyRad(tetaRad2PiInPoint(point))
    }

    private def _iteration(Ex:Double, M:Double):Double = {
      e*math.sin(Ex) + M
    }

    private val num_iterations = 30

    // Балк М.Б. Элементы динамики космического полета. Гл. III, параграф 3 "Решение уравнения Кеплера", стр. 111
    // http://pskgu.ru/ebooks/astro3/astro3_03_03.pdf
    def orbitalPointAfterTimeCCW(point1:DVec, time_sec:Long):DVec = {
      val t1 = tetaDeg360InPoint(point1)
      val time_from_r_p = travelTimeOnOrbitMsecCCW(0, t1/*, print_variant = true*/)
      val all_time = time_from_r_p/1000 + time_sec
      val M = 1/inv_n*all_time
      val E7 = (1 to num_iterations).foldLeft(M) {
        case (res, i) => _iteration(res, M)
      }
      val tg_half_teta_res_rad = math.sqrt((1 + e)/(1 - e))*math.tan(E7/2)
      val teta_res_rad = math.atan(tg_half_teta_res_rad)*2
      val teta_res_deg = teta_res_rad/math.Pi*180
      orbitalPointByTrueAnomalyDeg(teta_res_deg)
    }

    def orbitalPointAfterTimeCW(point1:DVec, time_sec:Long):DVec = {
      val t1 = tetaDeg360InPoint(point1)
      val time_from_r_p = travelTimeOnOrbitMsecCW(0, t1/*, print_variant = true*/)
      val all_time = t.toLong - (time_from_r_p/1000 + time_sec)
      val M = 1/inv_n*all_time
      val E7 = (1 to num_iterations).foldLeft(M) {
        case (res, i) => _iteration(res, M)
      }
      val tg_half_teta_res_rad = math.sqrt((1 + e)/(1 - e))*math.tan(E7/2)
      val teta_res_rad = math.atan(tg_half_teta_res_rad)*2
      val teta_res_deg = teta_res_rad/math.Pi*180
      orbitalPointByTrueAnomalyDeg(teta_res_deg)
    }
  }          

  class HyperbolaOrbit(
     val a:Double,                          // большая полуось
     val b:Double,
     val e:Double,                          // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
     val f:DVec,                            // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
     val center:DVec,                       // координаты центра
     val mu:Double) extends KeplerOrbit {
    val r_p = a*(e-1)                       // перигей
    val p = a*(e*e - 1)                     // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)

    val teta_deg_min = 180 - math.acos(1/e)/math.Pi*180         // разрешенные углы: от 0 до этого угла
    val teta_rad_min = teta_deg_min/180.0*math.Pi

    val teta_deg_max = -180 + math.acos(1/e)/math.Pi*180 + 360  // разрешенные углы: от этого угла до 360
    val teta_rad_max = teta_deg_max/180.0*math.Pi
    
    def tetaDeg360Valid(teta_deg360:Double):Boolean = {
      teta_deg360 <= teta_deg_min || teta_deg360 >= teta_deg_max
    }

    def tetaRad2PiValid(teta_rad2Pi:Double):Boolean = {
      teta_rad2Pi <= teta_rad_max || teta_rad2Pi >= teta_rad_min
    }

    val f_minus_center_n = (f - center).n
    val half_center = f - f_minus_center_n*(center.dist(f)*0.5)
    val inv_n = a*math.sqrt(a/mu)

    def strDefinition(prefix:String, planet_radius:Double, planet_velocity:DVec, planet_g:Double, ship_coord:DVec, ship_velocity:DVec):String = {
      val ccw = (ship_coord - f).perpendicular*(ship_velocity - planet_velocity) >= 0   // летим против часовой?
      val dir = if(ccw) "\u21b6" else "\u21b7"
      val r_p_approach = (ship_coord - f)*(ship_velocity - planet_velocity) >= 0
      val r_p_approach_str = if(r_p_approach) "удаляемся" else "приближаемся"
      if(r_p - planet_radius < 0 && !r_p_approach) {
        val y_axis = (ship_coord - f).n
        val v0y = (ship_velocity - planet_velocity) * y_axis
        val fall_time_msec = if (ccw) {
          val fall_teta_rad = -math.acos((p / (planet_radius + 3) - 1) / e) + 2 * math.Pi
          travelTimeOnOrbitMsecCCW(ship_coord, orbitalPointByTrueAnomalyRad(fall_teta_rad))
        } else {
          val fall_teta_rad = math.acos((p / (planet_radius + 3) - 1) / e)
          travelTimeOnOrbitMsecCW(ship_coord, orbitalPointByTrueAnomalyRad(fall_teta_rad))
        }

        val time_to_stop_at_full_power = math.abs(v0y / (1000000 / OrbitalKiller.ship.mass - planet_g))
        val fall_time_str = if (fall_time_msec < 30000) s"[r Поверхность через ${timeStr(fall_time_msec)} (${timeStr((time_to_stop_at_full_power * 1000l).toLong)})]" else s"Поверхность через ${timeStr(fall_time_msec)}"
        f"$prefix, незамкнутая, суборбитальная $dir, $r_p_approach_str, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, $fall_time_str"
      } else {
        f"$prefix, незамкнутая, $dir, $r_p_approach_str, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}"
      }
    }

    def tetaDeg360ByDir(dir:DVec):Double = {
      f_minus_center_n.deg360(dir)
    }

    def tetaDeg360InPoint(p:DVec) = tetaDeg360ByDir(p - f)

    def tetaRad2PiByDir(dir:DVec):Double = {
      f_minus_center_n.rad2Pi(dir)
    }

    def tetaRad2PiInPoint(p:DVec) = tetaRad2PiByDir(p - f)

    def tetaSignedRadByDir(dir:DVec) = f_minus_center_n.signedRad(dir)

    def tetaSignedRadInPoint(p:DVec) = tetaSignedRadByDir(p - f)

    def distanceByTrueAnomalyRad(teta_rad:Double) = {
      p/(1 + e*math.cos(teta_rad))
    }

    def distanceByTrueAnomalyDeg(teta_deg:Double) = {
      p/(1 + e*math.cos(teta_deg/180.0*math.Pi))
    }

    def distanceByDir(dir:DVec):Double = {
      p/(1 + e*math.cos(tetaRad2PiByDir(dir)))
    }

    def distanceInPoint(point:DVec) = {
      p/(1 + e*math.cos(tetaSignedRadInPoint(point)))
    }

    def orbitalPointByTrueAnomalyRad(teta_rad:Double) = {
      f + f_minus_center_n.rotateRad(teta_rad).n*distanceByTrueAnomalyRad(teta_rad)
    }

    def orbitalPointByTrueAnomalyDeg(teta_deg:Double) = {
      f + f_minus_center_n.rotateDeg(teta_deg).n*distanceByTrueAnomalyDeg(teta_deg)
    }

    def orbitalPointByDir(dir:DVec) = {
      f + dir.n*distanceByDir(dir)
    }

    def orbitalPointInPoint(point:DVec) = {
      val dir = point - f
      f + dir.n*distanceByDir(dir)
    }

    def _travelTimeOnOrbitMsecCCW(orbital_point1:DVec, orbital_point2:DVec):Long = {
      val r1 = (orbital_point1 - f).norma
      val r2 = (orbital_point2 - f).norma
      val s = orbital_point1.dist(orbital_point2)

      val chl1 = 1 + (r1 + r2 + s) / (2 * a)
      val chl2 = 1 + (r1 + r2 - s) / (2 * a)

      val l1 = math.log(chl1 + math.sqrt(chl1 * chl1 - 1))
      val l2 = math.log(chl2 + math.sqrt(chl2 * chl2 - 1))
      (inv_n * ((math.sinh(l1) - l1) - (math.sinh(l2) - l2))).toLong * 1000
    }

    def travelTimeOnOrbitMsecCCW(point1:DVec, point2:DVec, recalculate_points:Boolean = false):Long = {
      /*val t1 = tetaDeg360InPoint(point1)
      val t2 = tetaDeg360InPoint(point2)*/
      if(recalculate_points) {
        val orbital_point1 = orbitalPointInPoint(point1)
        val orbital_point2 = orbitalPointInPoint(point2)
        _travelTimeOnOrbitMsecCCW(orbital_point1, orbital_point2)
      } else {
        _travelTimeOnOrbitMsecCCW(point1, point2)
      }
    }

    def travelTimeOnOrbitMsecCCW(teta1Deg360:Double, teta2Deg360:Double):Long = {
      travelTimeOnOrbitMsecCCW(orbitalPointByTrueAnomalyDeg(teta1Deg360), orbitalPointByTrueAnomalyDeg(teta2Deg360))
    }

    def travelTimeOnOrbitMsecCW(point1:DVec, point2:DVec):Long = {
      travelTimeOnOrbitMsecCCW(point2, point1)
    }

    def travelTimeOnOrbitMsecCW(teta1Deg360:Double, teta2Deg360:Double):Long = {
      travelTimeOnOrbitMsecCCW(orbitalPointByTrueAnomalyDeg(teta2Deg360), orbitalPointByTrueAnomalyDeg(teta1Deg360))
    }

    /**
     * Орбитальная скорость в точке орбиты с данной истинной аномалией. Это скорость относительно притягивающего центра.
     * То есть, если планета в свою очередь движется по орбите, то чтобы получить абсолютную скорость, надо вычислять и прибавлять
     * эту скорость планеты в данный момент времени и так далее.
     * @param teta_rad - угол в радианах между радиус вектором на точку на орбите и направлением на перицентр.
     *                   Если против часовой стрелки - положительный, от 0 до pi, иначе отрицательный, от 0 до -pi
     *                   Рой А. Движение по орбитам. стр. 109
     *                   http://stu.alnam.ru/book_mor-60
     * @return
     */
    def orbitalVelocityByTrueAnomalyRad(teta_rad:Double) = {
      val orbital_point = orbitalPointByTrueAnomalyRad(teta_rad)
      val r = orbital_point.dist(f)
      math.sqrt(mu*(2/r + 1/a))
    }

    def orbitalVelocityByDir(dir:DVec) = {
      orbitalVelocityByTrueAnomalyRad(tetaRad2PiByDir(dir))
    }

    def orbitalVelocityInPoint(point:DVec) = {
      orbitalVelocityByTrueAnomalyRad(tetaRad2PiInPoint(point))
    }

    private def _iteration(Hx:Double, M:Double):Double = {
      def _arsh(z:Double) = math.log(z + math.sqrt(z*z + 1))
      _arsh((Hx + M)/e)
    }
    private val num_iterations = 60

    // Балк М.Б. Элементы динамики космического полета. Гл. III, параграф 3 "Решение уравнения Кеплера", стр. 111
    // http://pskgu.ru/ebooks/astro3/astro3_03_03.pdf
    // https://ru.wikipedia.org/wiki/Уравнение_Кеплера
    def orbitalPointAfterTimeCCW(point1:DVec, time_sec:Long):DVec = {
      val t1 = tetaDeg360InPoint(point1)
      val away_from_rp = teta_deg_min >= t1 && t1 >= 0
      if(away_from_rp) {
        val time_from_r_p_to_cur_point = travelTimeOnOrbitMsecCCW(0, t1 /*, print_variant = true*/)/1000
        val time_from_r_p = time_from_r_p_to_cur_point + time_sec
        val M = 1 / inv_n * time_from_r_p
        val H7 = (1 to num_iterations).foldLeft(M) {
          case (res, i) => _iteration(res, M)
        }
        val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(H7 / 2)
        val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
        val teta_res_deg = teta_res_rad / math.Pi * 180
        orbitalPointByTrueAnomalyDeg(teta_res_deg)
      } else {
        val time_from_cur_point_to_r_p = travelTimeOnOrbitMsecCCW(t1, 360 /*, print_variant = true*/)/1000
        if(time_sec >= time_from_cur_point_to_r_p) {
          val time_from_r_p = time_sec - time_from_cur_point_to_r_p
          val M = 1 / inv_n * time_from_r_p
          val H7 = (1 to num_iterations).foldLeft(M) {
            case (res, i) => _iteration(res, M)
          }
          val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(H7 / 2)
          val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
          val teta_res_deg = teta_res_rad / math.Pi * 180
          orbitalPointByTrueAnomalyDeg(teta_res_deg)
        } else {
          val time_from_r_p = time_from_cur_point_to_r_p - time_sec
          val M = 1 / inv_n * time_from_r_p
          val H7 = (1 to num_iterations).foldLeft(M) {
            case (res, i) => _iteration(res, M)
          }
          val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(H7 / 2)
          val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
          val teta_res_deg = 360 - teta_res_rad / math.Pi * 180
          orbitalPointByTrueAnomalyDeg(teta_res_deg)
        }
      }
    }

    def orbitalPointAfterTimeCW(point1:DVec, time_sec:Long):DVec = {
      val t1 = tetaDeg360InPoint(point1)
      val away_from_rp = 360 >= t1 && t1 >= teta_deg_max
      if(away_from_rp) {
        val time_from_r_p_to_cur_point = travelTimeOnOrbitMsecCW(360, t1/*, print_variant = true*/)/1000
        val time_from_r_p = time_from_r_p_to_cur_point + time_sec
        val M = 1/inv_n*time_from_r_p
        val H7 = (1 to num_iterations).foldLeft(M) {
          case (res, i) => _iteration(res, M)
        }
        val tg_half_teta_res_rad = math.sqrt((e + 1)/(e - 1))*math.tanh(H7/2)
        val teta_res_rad = math.atan(tg_half_teta_res_rad)*2
        val teta_res_deg = 360 - teta_res_rad/math.Pi*180
        orbitalPointByTrueAnomalyDeg(teta_res_deg)
      } else {
        val time_from_cur_point_to_r_p = travelTimeOnOrbitMsecCW(t1, 0/*, print_variant = true*/)/1000
        if(time_sec >= time_from_cur_point_to_r_p) {
          val time_from_r_p = time_sec - time_from_cur_point_to_r_p
          val M = 1/inv_n*time_from_r_p
          val H7 = (1 to num_iterations).foldLeft(M) {
            case (res, i) => _iteration(res, M)
          }
          val tg_half_teta_res_rad = math.sqrt((e + 1)/(e - 1))*math.tanh(H7/2)
          val teta_res_rad = math.atan(tg_half_teta_res_rad)*2
          val teta_res_deg = 360 - teta_res_rad/math.Pi*180
          orbitalPointByTrueAnomalyDeg(teta_res_deg)
        } else {
          val time_from_r_p = time_from_cur_point_to_r_p - time_sec
          val M = 1/inv_n*time_from_r_p
          val H7 = (1 to num_iterations).foldLeft(M) {
            case (res, i) => _iteration(res, M)
          }
          val tg_half_teta_res_rad = math.sqrt((e + 1)/(e - 1))*math.tanh(H7/2)
          val teta_res_rad = math.atan(tg_half_teta_res_rad)*2
          val teta_res_deg = teta_res_rad/math.Pi*180
          orbitalPointByTrueAnomalyDeg(teta_res_deg)
        }
      }
    }
  }

  /**
   * Вычисляет параметры орбиты
   * @param planet_mass - масса планеты
   * @param planet_coord - абсолютная координата планеты
   * @param body_mass - масса тела, вращающегося вокруг планеты
   * @param body_relative_coord - относительная координата тела (абсолютная координата тела минус абсолютная координата планеты)
   * @param body_relative_velocity - относительная линейная скорость тела (абсолютная скорость тела минус абсолютная скорость планеты)
   * @param G - гравитационная постоянная
   * @return объект Orbit, содержащий вычисленный набор параметров
   */
  def calculateOrbit(planet_mass:Double, planet_coord:DVec, body_mass:Double, body_relative_coord:DVec, body_relative_velocity:DVec, G:Double):KeplerOrbit = {
    //https://ru.wikipedia.org/wiki/Гравитационный_параметр
    val mu = (planet_mass + body_mass)*G // гравитационный параметр
    //val mu = planet_mass*G // гравитационный параметр

    //http://ru.wikipedia.org/wiki/Кеплеровы_элементы_орбиты
    //val a = body_relative_coord.norma*k/(2*k*k - body_relative_coord.norma*body_velocity.norma2)

    //https://en.wikipedia.org/wiki/Specific_orbital_energy
    val epsilon = body_relative_velocity.norma2/2 - mu/body_relative_coord.norma   // орбитальная энергия - сумма потенциальной и кинетической энергии тел, деленные на приведенную массу

    //http://ru.wikipedia.org/wiki/Большая_полуось
    val a = math.abs(mu/(2*epsilon))                             // большая полуось

    //http://en.wikipedia.org/wiki/Kepler_orbit
    val r_n = body_relative_coord.n
    val v_t = math.abs(body_relative_velocity*r_n.perpendicular)
    val p = math.pow(body_relative_coord.norma*v_t, 2)/mu                 // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)

    //http://ru.wikipedia.org/wiki/Эллипс
    val b = math.sqrt(math.abs(a*p))                             // малая полуось    
    
    if(epsilon < 0) {
      val e = math.sqrt(math.abs(1 - (b*b)/(a*a)))                 // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
      
      val c = a*e                                                  // фокальное расстояние (полурасстояние между фокусами)
      val r_p = a*(1 - e)                                          // перигей
      val r_a = a*(1 + e)                                          // апогей

      val t = 2 * math.Pi * math.sqrt(math.abs(a * a * a / mu))    // орбитальный период (период обращения по орбите, в секундах)

      val d1 = body_relative_coord.norma                                    // расстояние от тела до первого фокуса (планеты)
      val d2 = 2*a - d1                                            // расстояние до второго фокуса (свойства эллипса: d1+d2 = 2*a)
      val alpha = body_relative_coord.signedDeg(body_relative_velocity)        // угол между вектором скорости тела - касательным к эллипсу и направлением на первый фокус (свойство эллипса: угол между касательной и вектором на второй фокус такой же)
      val f1 = planet_coord                                                                // координаты первого фокуса - координаты планеты (она в фокусе эллипса-орбиты)
      val f2 = body_relative_velocity.rotateDeg(alpha).n*d2 + body_relative_coord + planet_coord    // координаты второго фокуса
      val center = (f2 - f1).n*c + f1                                                      // координаты центра орбиты-эллипса

      new EllipseOrbit(a, b, e, c, p, r_p, r_a, t, f1, f2, center, G*(planet_mass + body_mass))
    } else {
      // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
      val e = math.sqrt(math.abs(1 + (b * b) / (a * a)))

      val true_anomaly = math.abs(math.acos((a*(e*e-1) - body_relative_coord.norma)/(body_relative_coord.norma*e)))

      val counterclockwise = body_relative_coord.signedDeg(body_relative_velocity) > 0
      val moving_away = body_relative_velocity*body_relative_coord.n > 0

      val signum = if(counterclockwise) {
        if(moving_away) -1 else 1
      } else {
        if(moving_away) 1 else -1
      }

      val center = planet_coord - body_relative_coord.rotateRad(true_anomaly*signum).n*a*e

      new HyperbolaOrbit(a, b, e, planet_coord, center, mu)
    }
  }

  def equalGravityRadius(planet1:MutableBodyState, planet2:MutableBodyState):Double = {
    val A = planet1.coord.dist(planet2.coord)
    val X = planet1.mass/planet2.mass
    A*math.sqrt(X)/(math.sqrt(X) + 1)
  }

  /**
   * soi - sphere of influence. Радиус сферы вокруг планеты, в которой она оказывает наибольшее гравитационное влияние
   * @param smaller_planet_mass - масса малой планеты
   * @param semi_major_axis - главная полуось орбиты малой планеты
   * @param bigger_planet_mass - масса большей планеты или звезды, вокруг которой вращается малая планета
   * @return
   */
  def soi(smaller_planet_mass:Double, semi_major_axis:Double, bigger_planet_mass:Double):Double = {
    semi_major_axis*math.pow(smaller_planet_mass/bigger_planet_mass, 2.0/5)
  }

  def halfHillRadius(smaller_planet_mass:Double, semi_major_axis:Double, bigger_planet_mass:Double):Double = {
    0.5*semi_major_axis*math.pow(smaller_planet_mass/(3*bigger_planet_mass), 1.0/3)
  }

  def specificOrbitalEnergy(planet_mass:Double, planet_coord:DVec, body_mass:Double, body_relative_coord:DVec, body_relative_velocity:DVec, G:Double):Double = {
    val mu = (planet_mass + body_mass)*G // гравитационный параметр
    body_relative_velocity.norma2/2 - mu/body_relative_coord.norma
  }

  /**
   *
   * @param force - вектор силы
   * @param force_position_from_mass_center - точка приложения силы относительно центра масс
   * @param sin_angle - синус угла между вектором от центра масс до точки приложения силы и вектором силы
   * @return
   */
  def torque(force:DVec, force_position_from_mass_center:DVec, sin_angle:Double):Double = {
    force.norma*force_position_from_mass_center.norma*sin_angle
  }

  def torque(force:DVec, force_position_from_mass_center:DVec):Double = {
    val xf        = force_position_from_mass_center*force.p
    val sin_angle = xf/force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def torque(force:DVec, force_position:DVec, center:DVec):Double = {
    val force_position_from_mass_center = force_position - center
    val xf                              = force_position_from_mass_center*force.p
    val sin_angle                       = xf/force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def maxOption[T](l:Seq[T])(implicit o:Ordering[T]):Option[T] = if(l.isEmpty) None else Some(l.max(o))

  implicit class MyVec(v1:DVec) {
    def deg360(v2:DVec):Double = {
      val scalar = v1.perpendicular*v2
      if(scalar >= 0) v1.deg(v2) else 360 - v1.deg(v2)
    }

    def rad2Pi(v2:DVec):Double = {
      val scalar = v1.perpendicular*v2
      if(scalar >= 0) v1.rad(v2) else 2*math.Pi - v1.rad(v2)
    }
  }
}
