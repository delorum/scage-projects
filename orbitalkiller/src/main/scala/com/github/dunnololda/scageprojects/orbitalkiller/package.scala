package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{Body => Phys2dBody, BodyList => Phys2dBodyList, Collider => Phys2dCollider, DynamicShape => Phys2dShape, StaticBody => Phys2dStaticBody, _}
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

package object orbitalkiller {
  val G: Double = 6.6742867E-11

  // axis-aligned bounding box
  case class AABB(center: DVec, width: Double, height: Double) {
    val half_width = width / 2
    val half_height = height / 2

    def aabbCollision(b2: AABB): Boolean = {
      val d1 = math.abs(center.x - b2.center.x)
      (d1 < half_width + b2.half_width) && {
        val d2 = math.abs(center.y - b2.center.y)
        d2 < half_height + b2.half_height
      }
    }
  }

  def aabbCollision(b1: AABB, b2: AABB): Boolean = {
    val d1 = math.abs(b1.center.x - b2.center.x)
    (d1 < b1.half_width + b2.half_width) && {
      val d2 = math.abs(b1.center.y - b2.center.y)
      d2 < b1.half_height + b2.half_height
    }
  }

  sealed trait Shape {
    def aabb(center: DVec, rotation: Double): AABB

    def phys2dShape: Phys2dShape

    def wI: Double
  }

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
    val radius = math.sqrt(width * width + height * height)

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

  implicit class DVec2DoublePhys2dVector(v: DVec) {
    def toPhys2dVecD = new Vector2f(v.x, v.y)
  }

  case class PolygonShape(points: List[DVec], convex_parts: List[PolygonShape]) extends Shape {
    val radius = math.sqrt(points.map(p => p.norma2).max) * 2
    val points_center = points.sum / points.length
    val points_radius = math.sqrt(points.map(p => (p - points_center).norma2).max) * 2

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

    lazy val area = math.abs(0.5 * (points ::: List(points.head)).sliding(2).map {
      case List(p, p1) => (p.x + p1.x) * (p.y - p1.y)
    }.sum)
  }

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

  /**
    * Реализация http://en.wikipedia.org/wiki/Quadtree
    *
    * @param space     - начальное пространство, которое будем или не будем разделять
    * @param max_level - желаемое максимальное количество разбиений области на подобласти
    * @param target    - желаемое максимальное количество объектов в одной области
    * @param level     - текущий уровень разделения
    * @param spaces    - список пространств, который отдадим в качестве результата
    * @return
    */
  def splitSpace(space: Space, max_level: Int, target: Int, level: Int = 0, spaces: List[Space] = Nil): List[Space] = {
    if (space.bodies.length <= target) space :: spaces
    else if (level > max_level) space :: spaces
    else {
      space.quadSpaces.flatMap {
        case s => splitSpace(s, max_level, target, level + 1, spaces)
      }
    }
  }

  case class GeometricContactData(contact_point: DVec, normal: DVec, separation: Double)

  case class Contact(a: BodyState, b: BodyState, contact_point: DVec, normal: DVec, separation: Double)

  case class MutableContact(a: MutableBodyState, b: MutableBodyState, contact_point: DVec, normal: DVec, separation: Double) {
    def solveCollision(_dt: Double) {
      a.contacts += this
      b.contacts += this
      /*val a_prev_vel = a.vel
      val b_prev_vel = b.vel*/

      val e = /*1*//*0.9*/ math.min(a.restitution, b.restitution)

      // radii from centers of masses to contact
      val ra = contact_point - a.coord
      val rb = contact_point - b.coord

      val rv = b.vel + (b.ang_vel.toRad */ rb) - a.vel - (a.ang_vel.toRad */ ra) // Relative velocity

      val contactVel = rv * normal // Relative velocity along the normal
      if (contactVel <= 0) {
        // Do not resolve if velocities are separating
        val raCrossN = ra */ normal
        val rbCrossN = rb */ normal

        val invMassSum = a.invMass + b.invMass + (raCrossN * raCrossN) * a.invI + (rbCrossN * rbCrossN) * b.invI
        val j = (-(1.0f + e) * contactVel) / invMassSum ///contact_points.length
        val impulse = normal * j
        a.applyCollisionImpulse(-impulse, ra, _dt)
        b.applyCollisionImpulse(impulse, rb, _dt)

        val t = (rv + normal * (-rv * normal)).n
        val jt = (-(rv * t)) / invMassSum ///contact_points.length
        if (math.abs(jt) > 0.0001) {
          // Coulumb's law
          val tangentImpulse = if (math.abs(jt) < j * a.staticFriction) {
            t * jt
          } else {
            t * j * (-a.dynamicFriction)
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

    def positionalCorrection(tacts: Long = 0) {
      if (separation > 0.005) {
        val correction = separation / (a.invMass + b.invMass)
        if (correction != 0) {
          println(s"$tacts correction: separation=$separation correction_${a.index}=${-a.invMass * correction} correction_${b.index}=${b.invMass * correction}")
        }
        if (!a.is_static) a.coord += normal * (-a.invMass * correction)
        if (!b.is_static) b.coord += normal * b.invMass * correction
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

  def maybeCollisions(body1: MutableBodyState, body2: MutableBodyState): List[MutableContact] = {
    def _collide(pb1: Phys2dBody, pb2: Phys2dBody, collider: Phys2dCollider): List[GeometricContactData] = {
      //println(s"${body1.index} <-> ${body2.index}")
      val num_contacts = collider.collide(contacts, pb1, pb2)
      if (num_contacts == 0) Nil
      else {
        num_contacts match {
          case 1 =>
            val contact_point = contacts(0).getPosition.toDVec
            val normal = contacts(0).getNormal.toDVec
            List(GeometricContactData(contact_point, normal, math.abs(contacts(0).getSeparation)))
          case 2 =>
            val contact_points = List(contacts(0).getPosition.toDVec, contacts(1).getPosition.toDVec)
            val contact_point = contact_points.sum / contact_points.length
            val normal = contacts(0).getNormal.toDVec
            val separation = math.max(math.abs(contacts(0).getSeparation), math.abs(contacts(1).getSeparation))
            List(GeometricContactData(contact_point, normal, separation))
          case _ => Nil
        }
      }
    }
    def _circlesTouches(c1: DVec, r1: Double, c2: DVec, r2: Double): Boolean = {
      val totalRad = r1 + r2
      val dx = math.abs(c2.x - c1.x)
      if (dx > totalRad) {
        false
      } else {
        val dy = math.abs(c2.y - c1.y)
        if (dy > totalRad) {
          false
        } else {
          val totalRad2 = totalRad * totalRad
          totalRad2 >= ((dx * dx) + (dy * dy))
        }
      }
    }
    if (!body1.aabb.aabbCollision(body2.aabb)) {
      Nil
    } else {
      body1.shape match {
        case c1: CircleShape =>
          body2.shape match {
            case c2: CircleShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, circle_circle_collider).map(gcd => {
                MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
              })
            case l2: LineShape =>
              _collide(body2.phys2dBody, body1.phys2dBody, line_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
            case b2: BoxShape =>
              if (_circlesTouches(body1.coord, c1.radius, body2.coord, b2.radius)) {
                _collide(body1.phys2dBody, body2.phys2dBody, circle_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
              } else {
                Nil
              }
            case p2: PolygonShape =>
              if (_circlesTouches(body1.coord, c1.radius, body2.coord, p2.radius)) {
                if (p2.convex_parts.isEmpty) {
                  _collide(body2.phys2dBody, body1.phys2dBody, polygon_circle_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                  })
                } else {
                  p2.convex_parts.flatMap(p => {
                    if (_circlesTouches(body1.coord, c1.radius, body2.coord + p.points_center.rotateDeg(body2.ang), p.points_radius)) {
                      _collide(body2.phys2dBodyWithShape(p), body1.phys2dBody, polygon_circle_collider).map(gcd => {
                        MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                      })
                    } else {
                      Nil
                    }
                  })
                }
              } else {
                Nil
              }
            case _ => Nil
          }
        case l1: LineShape =>
          body2.shape match {
            case c2: CircleShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, line_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case l2: LineShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, line_line_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case b2: BoxShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case p2: PolygonShape =>
              if (p2.convex_parts.isEmpty) {
                _collide(body1.phys2dBody, body2.phys2dBody, line_polygon_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
              } else {
                p2.convex_parts.flatMap(p => {
                  _collide(body1.phys2dBody, body2.phys2dBodyWithShape(p), line_polygon_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                })
              }
            case _ => Nil
          }
        case b1: BoxShape =>
          body2.shape match {
            case c2: CircleShape =>
              if (_circlesTouches(body1.coord, b1.radius, body2.coord, c2.radius)) {
                _collide(body1.phys2dBody, body2.phys2dBody, box_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
              } else {
                Nil
              }
            case l2: LineShape =>
              _collide(body2.phys2dBody, body1.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
            case b2: BoxShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, box_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case p2: PolygonShape =>
              if (_circlesTouches(body1.coord, b1.radius, body2.coord, p2.radius)) {
                if (_circlesTouches(body1.coord, b1.radius, body2.coord, p2.radius)) {
                  if (p2.convex_parts.isEmpty) {
                    _collide(body2.phys2dBody, body1.phys2dBody, polygon_box_collider).map(gcd => {
                      MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                    })
                  } else {
                    p2.convex_parts.flatMap(p => {
                      if (_circlesTouches(body1.coord, b1.radius, body2.coord + p.points_center.rotateDeg(body2.ang), p.points_radius)) {
                        _collide(body2.phys2dBodyWithShape(p), body1.phys2dBody, polygon_box_collider).map(gcd => {
                          MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                        })
                      } else {
                        Nil
                      }
                    })
                  }
                } else {
                  Nil
                }
              } else {
                Nil
              }
            case _ => Nil
          }
        case p1: PolygonShape =>
          body2.shape match {
            case c2: CircleShape =>
              if (_circlesTouches(body1.coord, p1.radius, body2.coord, c2.radius)) {
                if (p1.convex_parts.isEmpty) {
                  _collide(body1.phys2dBody, body2.phys2dBody, polygon_circle_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                } else {
                  p1.convex_parts.flatMap(p => {
                    if (_circlesTouches(body1.coord + p.points_center.rotateDeg(body1.ang), p.points_radius, body2.coord, c2.radius)) {
                      _collide(body1.phys2dBodyWithShape(p), body2.phys2dBody, polygon_circle_collider).map(gcd => {
                        MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                      })
                    } else {
                      Nil
                    }
                  })
                }
              } else {
                Nil
              }
            case l2: LineShape =>
              if (p1.convex_parts.isEmpty) {
                _collide(body2.phys2dBody, body1.phys2dBody, line_polygon_collider).map(gcd => {
                  MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                })
              } else {
                p1.convex_parts.flatMap(p => {
                  _collide(body2.phys2dBody, body1.phys2dBodyWithShape(p), line_polygon_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation)
                  })
                })
              }
            case b2: BoxShape =>
              if (_circlesTouches(body1.coord, p1.radius, body2.coord, b2.radius)) {
                if (p1.convex_parts.isEmpty) {
                  _collide(body1.phys2dBody, body2.phys2dBody, polygon_box_collider).map(gcd => {
                    MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                  })
                } else {
                  p1.convex_parts.flatMap(p => {
                    if (_circlesTouches(body1.coord + p.points_center.rotateDeg(body1.ang), p.points_radius, body2.coord, b2.radius)) {
                      _collide(body1.phys2dBodyWithShape(p), body2.phys2dBody, polygon_box_collider).map(gcd => {
                        MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                      })
                    } else {
                      Nil
                    }
                  })
                }
              } else {
                Nil
              }
            case p2: PolygonShape =>
              if (_circlesTouches(body1.coord, p1.radius, body2.coord, p2.radius)) {
                if (p1.convex_parts.isEmpty) {
                  // у p1 нет convex_parts
                  if (p2.convex_parts.isEmpty) {
                    // convex_parts ни у кого нет
                    _collide(body1.phys2dBody, body2.phys2dBody, polygon_polygon_collider).map(gcd => {
                      MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                    })
                  } else {
                    // convex_parts есть только у p2
                    p2.convex_parts.flatMap(p_2 => {
                      if (_circlesTouches(body1.coord, p1.radius, body2.coord + p_2.points_center.rotateDeg(body2.ang), p_2.points_radius)) {
                        _collide(body1.phys2dBody, body2.phys2dBodyWithShape(p_2), polygon_polygon_collider).map(gcd => {
                          MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                        })
                      } else {
                        Nil
                      }
                    })
                  }
                } else {
                  // у p1 есть convex_parts
                  p1.convex_parts.flatMap(p_1 => {
                    if (p2.convex_parts.isEmpty) {
                      // у p2 нет convex_parts
                      if (_circlesTouches(body1.coord + p_1.points_center.rotateDeg(body1.ang), p_1.points_radius, body2.coord, p2.radius)) {
                        _collide(body1.phys2dBodyWithShape(p_1), body2.phys2dBody, polygon_polygon_collider).map(gcd => {
                          MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                        })
                      } else {
                        Nil
                      }
                    } else {
                      // convex_parts есть у всех
                      p2.convex_parts.flatMap(p_2 => {
                        if (_circlesTouches(body1.coord + p_1.points_center.rotateDeg(body1.ang), p_1.points_radius, body2.coord + p_2.points_center.rotateDeg(body2.ang), p_2.points_radius)) {
                          _collide(body1.phys2dBodyWithShape(p_1), body2.phys2dBodyWithShape(p_2), polygon_polygon_collider).map(gcd => {
                            MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
                          })
                        } else {
                          Nil
                        }
                      })
                    }
                  })
                }
              } else {
                Nil
              }
            case _ => Nil
          }
        case _ => Nil
      }
    }
  }

  case class BodyState(index: Int,
                       mass: Double,
                       acc: DVec = DVec.zero,
                       vel: DVec = DVec.zero,
                       coord: DVec,
                       ang_acc: Double = 0,
                       ang_vel: Double = 0,
                       ang: Double = 0,
                       shape: Shape,
                       is_static: Boolean = false,
                       restitution: Double = 0.2, // elasticity or restitution: 0 - inelastic, 1 - perfectly elastic, (va2 - vb2) = -e*(va1 - vb1)
                       staticFriction: Double = 0.5,
                       dynamicFriction: Double = 0.3,
                       collisions: List[Contact] = Nil,
                       collisions_dacc: DVec = DVec.zero,
                       collisions_dvel: DVec = DVec.zero,
                       collisions_d_ang_acc: Double = 0.0,
                       collisions_d_ang_vel: Double = 0.0,
                       is_bullet: Boolean = false) {
    val aabb = shape.aabb(coord, ang)
    val I = mass * shape.wI
    val invMass = if (is_static || mass == 0) 0 else 1.0 / mass
    val invI = if (is_static || I == 0) 0 else 1.0 / I

    def toMutableBodyState = new MutableBodyState(this)
  }

  class MutableBodyState(body: BodyState) {
    var active: Boolean = true

    val init_aabb = body.aabb
    val restitution = body.restitution

    val staticFriction = body.staticFriction
    val dynamicFriction = body.dynamicFriction
    val is_static = body.is_static
    val is_bullet = body.is_bullet
    val shape = body.shape
    val index = body.index
    val strIndex = index.toString

    private var _mass = body.mass

    def mass = _mass

    def mass_=(m: Double): Unit = {
      _mass = m
      _invMass = if (is_static || _mass == 0) 0.0 else 1.0 / _mass
      _I = _mass * shape.wI
      _invI = if (is_static || _I == 0) 0.0 else 1.0 / _I
    }

    private var _invMass = if (is_static || _mass == 0) 0.0 else 1.0 / _mass

    def invMass = _invMass

    private var _I = _mass * shape.wI

    def I = _I

    private var _invI = if (is_static || _I == 0) 0.0 else 1.0 / _I

    def invI = _invI

    // ===========================================

    var acc: DVec = DVec.zero

    var vel: DVec = body.vel
    /*def vel = _vel
    def vel_=(new_vel:DVec): Unit = {
      if(new_vel.x.abs > 300000000l || new_vel.y.abs > 300000000l) {
        throw new Exception("vel is set to infinity!")
      }
      _vel = new_vel
    }*/

    var coord: DVec = body.coord
    /*def coord = _coord
    def coord_=(new_coord:DVec): Unit = {
      if(new_coord.x.isNaN || new_coord.y.isNaN) {
        throw new Exception("coord is set to infinity!")
      }
      _coord = new_coord
    }*/

    var ang_acc: Double = 0.0
    var ang_vel: Double = body.ang_vel
    var ang: Double = body.ang

    var dacc: DVec = DVec.zero
    var dvel: DVec = DVec.zero
    var d_ang_acc: Double = 0.0
    var d_ang_vel: Double = 0.0

    val contacts = ArrayBuffer[MutableContact]()

    def init(): Unit = {
      acc = DVec.zero
      ang_acc = 0.0
      dacc = DVec.zero
      dvel = DVec.zero
      d_ang_acc = 0.0
      d_ang_vel = 0.0
      contacts.clear()
      aabb = shape.aabb(coord, ang)
    }

    var aabb = shape.aabb(coord, ang)

    def phys2dBody = {
      if (is_static) {
        val x = new Phys2dStaticBody(strIndex, shape.phys2dShape)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.setUserData((index, shape))
        x
      } else {
        val x = new Phys2dBody(strIndex, shape.phys2dShape, mass)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.adjustVelocity(vel.toPhys2dVecD)
        x.adjustAngularVelocity(ang_vel.toRad)
        x.setUserData((index, shape))
        x
      }
    }

    def phys2dBodyWithShape(shape: Shape) = {
      if (is_static) {
        val x = new Phys2dStaticBody(strIndex, shape.phys2dShape)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.setUserData((index, shape))
        x
      } else {
        val x = new Phys2dBody(strIndex, shape.phys2dShape, mass)
        x.setPosition(coord.x, coord.y)
        x.setRotation(ang.toRad)
        x.adjustVelocity(vel.toPhys2dVecD)
        x.adjustAngularVelocity(ang_vel.toRad)
        x.setUserData((index, shape))
        x
      }
    }

    def applyCollisionImpulse(impulse: DVec, contactVector: DVec, dt: Double) {
      if (!is_static) {
        val x = impulse * invMass
        acc += x / dt
        dacc += x / dt
        vel += x
        dvel += x


        val y = (invI * (contactVector */ impulse)).toDeg
        ang_acc += y / dt
        d_ang_acc += y / dt
        ang_vel += y
        d_ang_vel += y
      }
    }

    def toImmutableBodyState: BodyState = body.copy(
      mass = mass,
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

    def copy: MutableBodyState = {
      val mb = new MutableBodyState(toImmutableBodyState)
      mb
    }

    override def toString = s"MutableBodyState($index)"

    def saveData: String = s"$index ${acc.x}:${acc.y} ${vel.x}:${vel.y} ${coord.x}:${coord.y} $ang_acc $ang_vel $ang"

    lazy val polygonShape = shape match {
      case p: PolygonShape => Some(p)
      case _ => None
    }
  }

  implicit class Phys2dBody2BodyState(pb: Phys2dBody) {
    def toBodyState: Option[BodyState] = {
      pb.getUserData match {
        case (index: Int, shape: Shape) =>
          Some(BodyState(
            index = index,
            mass = pb.getMass,
            acc = DVec.dzero,
            vel = DVec(pb.getVelocity.getX, pb.getVelocity.getY),
            coord = DVec(pb.getPosition.getX, pb.getPosition.getY),
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

  implicit class Phys2dBodyList2List(pbl: Phys2dBodyList) {
    def toList: List[(Phys2dBody, BodyState)] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield (pb, bs)).toList
    }

    def toBodyStateList: List[BodyState] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield bs).toList
    }
  }

  def maxOption[T](l: Seq[T])(implicit o: Ordering[T]): Option[T] = if (l.isEmpty) None else Some(l.max(o))

  def checkAllConditions(conditions: (() => Boolean)*): Boolean = {
    if (conditions.isEmpty) true
    else if (conditions.head()) checkAllConditions(conditions.tail: _*)
    else false
  }
}
