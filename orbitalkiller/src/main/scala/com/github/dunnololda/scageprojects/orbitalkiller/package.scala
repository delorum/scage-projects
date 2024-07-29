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

    lazy val area = math.abs(0.5*(points ::: List(points.head)).sliding(2).map {
      case List(p, p1) => (p.x + p1.x)*(p.y - p1.y)
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
    var active:Boolean = true

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

  // структура хранит и по необходимости довычисляет набор простых чисел. Вычисление производится методом решета Эратосфена
  object erat2 {
    private var eratl = (2L, Seq[Long](2))

    def apply(n: Long): Seq[Long] = {
      def _erat(_n: Long, l: Seq[Long], p: Long): Seq[Long] = {
        println(p)
        if (p * p > _n) l
        else {
          val m = l.filterNot(x => x > p && x % p == 0)
          _erat(_n, m, m.find(_ > p).get)
        }
      }

      if (n > eratl._1) {
        println("generating primes...")
        val new_eratl_2 = _erat(n * 2, eratl._2 ++ (eratl._1 + 1 to n * 2), 2)
        eratl = (n * 2, new_eratl_2)
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
  def isPrime(n: Long, sqrt_n: Long = -1l, cur_p: Long = 1l): Boolean = {
    if (sqrt_n == -1l) isPrime(n, math.sqrt(n).toLong, cur_p)
    else {
      if (cur_p >= sqrt_n) true
      else {
        val new_primes_portion = erat2(math.min(cur_p * 2, sqrt_n)).dropWhile(_ <= cur_p)
        if (new_primes_portion.isEmpty) true
        else {
          val res = new_primes_portion.forall(n % _ != 0)
          if (res) isPrime(n, sqrt_n, new_primes_portion.last)
          else false
        }
      }
    }
  }

  val pfacts = mutable.HashMap[Long, List[Long]]()

  // возвращает список простых чисел, произведение которых равно данному числу n
  def primeFactors3(n: Long): List[Long] = {
    def _primeFactors3(_n: Long, facs: List[Long] = Nil): List[Long] = {
      pfacts.get(_n).map(x => x ::: facs).getOrElse(
        erat2(math.sqrt(_n).toLong).find(_n % _ == 0) match {
          case Some(m) => _primeFactors3(_n / m, m :: facs)
          case None => _n :: facs
        }
      )
    }
    _primeFactors3(n)
  }

  // возвращает список простых чисел, произведение которых равно данному числу n, кеширует результат для ускорения последующих запросов
  def primeFactors4(n: Long) = {
    pfacts.getOrElseUpdate(n, primeFactors3(n))
  }

  // возвращает список делителей данного числа n (простых и непростых)
  def factors4(n: Long) = {
    val res23 = primeFactors4(n)
    (1L :: (1 to res23.length).flatMap(i => res23.combinations(i).map(_.product)).toList).filterNot(_ == n) ::: n :: Nil
  }

  private val facts = mutable.HashMap[Long, List[Long]]()

  // возвращает список делителей, кеширует результат
  def factors5(n: Long) = {
    facts.getOrElseUpdate(n, factors4(n))
  }

  case class CollisionData(
                            collided_body: BodyState,
                            contact_point: DVec,
                            normal: DVec,
                            separation: Double,
                            contacts: List[net.phys2d.raw.Contact], new_velocity: DVec, new_angular_velocity: Double)

  def mutableSystemEvolution(mutable_system: Seq[(MutableBodyState, Seq[MutableBodyState])],
                             base_dt: Double, // в секундах
                             force: (MutableBodyState, Seq[MutableBodyState]) => DVec = (body, other_bodies) => DVec.dzero,
                             torque: (MutableBodyState, Seq[MutableBodyState]) => Double = (body, other_bodies) => 0.0,
                             enable_collisions: Boolean = true,
                             system_center: DVec = DVec.zero): Unit = {
    mutable_system.foreach(_._1.init())

    val collisions = if (enable_collisions) {
      val x = splitSpace(new Space(mutable_system.map(_._1), system_center), 5, 2)
      for {
        space <- x
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx + 1)
        if !b1.is_static || !b2.is_static
        c <- maybeCollisions(b1, b2)
      } yield c
    } else Nil

    // Integrate forces first part
    mutable_system.foreach { case (mb, other_mutable_bodies) =>
      if (!mb.is_static) {
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
    if (collisions.nonEmpty) collisions.foreach(c => c.solveCollision(base_dt))

    // Integrate velocities and forces last part
    mutable_system.foreach { case (mb2, other_mutable_bodies2) =>
      if (!mb2.is_static) {
        mb2.coord += mb2.vel * base_dt
        mb2.ang = correctAngle((mb2.ang + mb2.ang_vel * base_dt) % 360)
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
    if (collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())
  }

  def systemEvolutionFrom(dt: => Double, // в секундах, может быть больше либо равно base_dt - обеспечивается ускорение времени
                          maxMultiplier: => Int = 1,
                          base_dt: Double, // в секундах
                          force: (Long, BodyState, List[BodyState]) => DVec = (time, body, other_bodies) => DVec.dzero,
                          torque: (Long, BodyState, List[BodyState]) => Double = (time, body, other_bodies) => 0.0,
                          changeFunction: (Long, List[BodyState]) => (Long, List[BodyState]) = (time, bodies) => (time, bodies),
                          enable_collisions: Boolean = true)
                         (current_state: (Long, List[BodyState])): Stream[(Long, List[BodyState])] = {
    def _step(state: (Long, List[BodyState]), _dt: Double, steps: Int): (Long, List[BodyState]) = {
      val (tacts, bodies) = changeFunction(state._1, state._2)
      val next_tacts = tacts + steps

      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-the-basics-and-impulse-resolution--gamedev-6331
      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-friction-scene-and-jump-table--gamedev-7756
      // http://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-oriented-rigid-bodies--gamedev-8032
      // http://www.niksula.hut.fi/~hkankaan/Homepages/gravity.html

      val mutable_bodies = bodies.map(_.toMutableBodyState)

      val collisions = if (enable_collisions && _dt == base_dt) {
        for {
          space <- splitSpace(new Space(mutable_bodies, DVec.zero), 5, 3)
          if space.bodies.length > 1
          (b1, idx) <- space.bodies.zipWithIndex.init
          b2 <- space.bodies.drop(idx + 1)
          if !b1.is_static || !b2.is_static
          c <- maybeCollisions(b1, b2)
        } yield c
      } else Nil

      val mb_and_others = for {
        (mb, idx) <- mutable_bodies.zipWithIndex
        other_mutable_bodies = mutable_bodies.take(idx) ::: mutable_bodies.drop(idx + 1)
      } yield (mb, other_mutable_bodies)

      // Integrate forces first part
      mb_and_others.foreach { case (mb, other_mutable_bodies) =>
        if (!mb.is_static) {
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
      if (collisions.nonEmpty) collisions.foreach(c => c.solveCollision(_dt))

      // Integrate velocities and forces last part
      mb_and_others.foreach { case (mb2, other_mutable_bodies2) =>
        if (!mb2.is_static) {
          val b2 = mb2.toImmutableBodyState
          mb2.coord += mb2.vel * _dt
          mb2.ang = correctAngle((mb2.ang + mb2.ang_vel * _dt) % 360)
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
      if (collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

      val x = collisions.groupBy(mc => mc.a.index).map(kv => (kv._1, kv._2.map(_.toImmutableForA)))
      val y = collisions.groupBy(mc => mc.b.index).map(kv => (kv._1, kv._2.map(_.toImmutableForB)))
      val z = (x.keySet ++ y.keySet).map(body_index => (body_index, x.getOrElse(body_index, Nil) ::: y.getOrElse(body_index, Nil))).toMap

      val next_bodies = mutable_bodies.map(mb => mb.toImmutableBodyState.copy(collisions = z.getOrElse(mb.index, Nil)))

      (next_tacts, next_bodies)
    }
    val cur_dt = dt
    val max_multiplier = maxMultiplier
    val steps = math.max((cur_dt / base_dt).toInt, 1)

    val next_state = if (steps < max_multiplier) {
      _step(current_state, cur_dt, steps)
    } else {
      if (max_multiplier == 1) {
        (1 to steps).foldLeft(current_state) {
          case (state, step) =>
            _step(state, base_dt, 1)
        }
      } else {
        // пусть maxMultiplier = 450, а мы хотим ускорение 1000
        // тогда подбираем ближайший multiplier меньше 450, на который делится 1000 без остатка
        val multiplier = factors5(steps).filter(_ <= max_multiplier).max.toInt
        val steps2 = steps / multiplier
        (1 to steps2).foldLeft(current_state) {
          case (state, step) =>
            _step(state, base_dt * multiplier, multiplier)
        }
      }
    }

    next_state #:: systemEvolutionFrom(dt, maxMultiplier, base_dt, force, torque, changeFunction, enable_collisions)(next_state)
  }

  def gravityForce(body1_coord: DVec, body1_mass: Double, body2_coord: DVec, body2_mass: Double, G: Double): DVec = {
    (body1_coord - body2_coord).n * G * body1_mass * body2_mass / body1_coord.dist2(body2_coord)
  }

  private def _satelliteSpeed(from_planet_to_body: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val sat_speed = from_planet_to_body.p * math.sqrt(G * planet_mass / from_planet_to_body.norma)
    if (!counterclockwise) planet_velocity + sat_speed * (-1)
    else planet_velocity + sat_speed
  }

  def satelliteSpeed(body_coord: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def satelliteSpeed(body_coord: DVec, body_velocity: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular * (body_velocity - planet_velocity) >= 0
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  private def _escapeVelocity(from_planet_to_body: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val esc_speed = from_planet_to_body.p * math.sqrt(G * planet_mass / from_planet_to_body.norma) * math.sqrt(2)
    if (!counterclockwise) planet_velocity + esc_speed * (-1)
    else planet_velocity + esc_speed
  }

  def escapeVelocity(body_coord: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def escapeVelocity(body_coord: DVec, body_velocity: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular * (body_velocity - planet_velocity) >= 0
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  /**
   * Возвращает скорость, которую надо иметь кораблю, чтобы быть в перигее или апогее орбиты с заданными параметрами
   * @param perigee_coord - текущая координата корабля, и это будет координата перигея или апогея
   * @param apogee_diff - возвышение противоположной точки: насколько апогей выше перигея (если мы в перигее), либо наоборот, насколько перигей ниже,
   *                    положительное или отрицательное число тут
   * @param planet_coord - координата планеты
   * @param planet_velocity - скорость планеты
   * @param planet_mass - масса планеты
   * @param G - гравитационная постоянная
   * @return двумерный вектор скорости
   */
  def speedToHaveOrbitWithParams(perigee_coord: DVec, apogee_diff: Double, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, ccw: Boolean = true): DVec = {
    val r_p = perigee_coord.dist(planet_coord)
    val r_a = r_p + apogee_diff
    val mu = planet_mass * G
    if(ccw) {
      planet_velocity + math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    } else {
      planet_velocity - math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    }
  }

  def mOrKmOrMKm(meters: Number): String = {
    val d = meters.doubleValue()
    val absd = math.abs(d)
    if (absd < 1000) {
      f"$d%.2f м"
    } else if (absd < 1000000) {
      f"${d / 1000}%.2f км"
    } else if (absd < 1000000000) {
      f"${d / 1000000}%.2f тыс. км"
    } else {
      f"${d / 1000000000}%.2f млн. км"
    }
  }

  def msecOrKmsec(msec: Number): String = {
    InterfaceHolder.msecOrKmH.selectedVariant match {
      case 0 => // m/sec
        if (math.abs(msec.doubleValue()) < 1000) {
          f"${msec.doubleValue()}%.2f м/сек"
        } else {
          f"${msec.doubleValue() / 1000}%.2f км/сек"
        }
      case 1 => // km/h
        f"${msec.doubleValue() * 3.6}%.2f км/ч"
      case _ =>
        ""
    }
  }

  def newtonOrKilonewton(newton: Number): String = {
    if (math.abs(newton.doubleValue()) < 1000) f"${newton.doubleValue()}%.2f Н" else f"${newton.doubleValue() / 1000}%.2f кН"
  }

  def gOrKg(kg: Double): String = {
    if (kg < 1) f"${kg * 1000}%.0f г"
    else {
      if (kg % 1 == 0) f"$kg%.0f кг"
      else f"${kg.toInt} кг ${(kg - kg.toInt) * 1000}%.0f г"
    }
  }

  /**
   * Ускорение
   * @param msec - метры в секунду
   * @return
   */
  def msec2OrKmsec2(msec: Number): String = {
    if (math.abs(msec.doubleValue()) < 1000) f"${msec.doubleValue()}%.2f м/сек^2" else f"${msec.doubleValue() / 1000}%.2f км/сек^2"
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
  def calculateOrbit(planet_mass: Double, planet_coord: DVec, body_mass: Double, body_relative_coord: DVec, body_relative_velocity: DVec, G: Double): KeplerOrbit = {
    //https://ru.wikipedia.org/wiki/Гравитационный_параметр
    //val mu = (planet_mass + body_mass) * G // гравитационный параметр
    val mu = planet_mass*G // гравитационный параметр

    //http://ru.wikipedia.org/wiki/Кеплеровы_элементы_орбиты
    //val a = body_relative_coord.norma*k/(2*k*k - body_relative_coord.norma*body_velocity.norma2)

    //https://en.wikipedia.org/wiki/Specific_orbital_energy
    val epsilon = body_relative_velocity.norma2 / 2 - mu / body_relative_coord.norma // орбитальная энергия - сумма потенциальной и кинетической энергии тел, деленные на приведенную массу

    //http://ru.wikipedia.org/wiki/Большая_полуось
    val a = math.abs(mu / (2 * epsilon)) // большая полуось

    //http://en.wikipedia.org/wiki/Kepler_orbit
    val r_n = body_relative_coord.n
    val v_t = math.abs(body_relative_velocity * r_n.perpendicular)
    val p = math.pow(body_relative_coord.norma * v_t, 2) / mu // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)

    //http://ru.wikipedia.org/wiki/Эллипс
    val b = math.sqrt(math.abs(a * p)) // малая полуось

    if (epsilon < 0) {
      val e = math.sqrt(math.abs(1 - (b * b) / (a * a))) // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)

      val c = a * e // фокальное расстояние (полурасстояние между фокусами)
      val r_p = a * (1 - e) // перигей
      val r_a = a * (1 + e) // апогей

      val t = 2 * math.Pi * math.sqrt(math.abs(a * a * a / mu)) // орбитальный период (период обращения по орбите, в секундах)

      val d1 = body_relative_coord.norma // расстояние от тела до первого фокуса (планеты)
      val d2 = 2 * a - d1 // расстояние до второго фокуса (свойства эллипса: d1+d2 = 2*a)
      val alpha = body_relative_coord.signedDeg(body_relative_velocity) // угол между вектором скорости тела - касательным к эллипсу и направлением на первый фокус (свойство эллипса: угол между касательной и вектором на второй фокус такой же)
      val f1 = planet_coord // координаты первого фокуса - координаты планеты (она в фокусе эллипса-орбиты)
      val f2 = body_relative_velocity.rotateDeg(alpha).n * d2 + body_relative_coord + planet_coord // координаты второго фокуса
      val center = (f2 - f1).n * c + f1 // координаты центра орбиты-эллипса

      /*if(a == d1 || f2 == f1) {
        println("orbit is round")
      }

      if(a == 0 || b == 0 || e.isNaN || c.isNaN || p == 0 || r_p.isNaN || r_a.isNaN || t == 0) {
        throw new Exception("ellipse orbit calculation failed")
      }*/

      new EllipseOrbit(a, b, e, c, p, r_p, r_a, t, f1, f2, center, G * (planet_mass + body_mass))
    } else {
      // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
      val e = math.sqrt(math.abs(1 + (b * b) / (a * a)))

      val true_anomaly = math.abs(((a * (e * e - 1) - body_relative_coord.norma) / (body_relative_coord.norma * e)).myacos)

      val counterclockwise = body_relative_coord.signedDeg(body_relative_velocity) > 0
      val moving_away = body_relative_velocity * body_relative_coord.n > 0

      val signum = if (counterclockwise) {
        if (moving_away) -1 else 1
      } else {
        if (moving_away) 1 else -1
      }

      val center = planet_coord - body_relative_coord.rotateRad(true_anomaly * signum).n * a * e

      new HyperbolaOrbit(a, b, e, planet_coord, center, mu)
    }
  }

  def equalGravityRadius(planet1: MutableBodyState, planet2: MutableBodyState): Double = {
    val A = planet1.coord.dist(planet2.coord)
    val X = planet1.mass / planet2.mass
    A * math.sqrt(X) / (math.sqrt(X) + 1)
  }

  /**
   * soi - sphere of influence. Радиус сферы вокруг планеты, в которой она оказывает наибольшее гравитационное влияние
   * @param smaller_planet_mass - масса малой планеты
   * @param semi_major_axis - главная полуось орбиты малой планеты
   * @param bigger_planet_mass - масса большей планеты или звезды, вокруг которой вращается малая планета
   * @return
   */
  def soi(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    semi_major_axis * math.pow(smaller_planet_mass / bigger_planet_mass, 2.0 / 5)
  }

  def halfHillRadius(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    0.5 * semi_major_axis * math.pow(smaller_planet_mass / (3 * bigger_planet_mass), 1.0 / 3)
  }

  def specificOrbitalEnergy(planet_mass: Double, planet_coord: DVec, body_mass: Double, body_relative_coord: DVec, body_relative_velocity: DVec, G: Double): Double = {
    val mu = (planet_mass + body_mass) * G // гравитационный параметр
    body_relative_velocity.norma2 / 2 - mu / body_relative_coord.norma
  }

  /**
   *
   * @param force - вектор силы
   * @param force_position_from_mass_center - точка приложения силы относительно центра масс
   * @param sin_angle - синус угла между вектором от центра масс до точки приложения силы и вектором силы
   * @return
   */
  def torque(force: DVec, force_position_from_mass_center: DVec, sin_angle: Double): Double = {
    force.norma * force_position_from_mass_center.norma * sin_angle
  }

  def torque(force: DVec, force_position_from_mass_center: DVec): Double = {
    val xf = force_position_from_mass_center * force.p
    val sin_angle = xf / force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def torque(force: DVec, force_position: DVec, center: DVec): Double = {
    val force_position_from_mass_center = force_position - center
    val xf = force_position_from_mass_center * force.p
    val sin_angle = xf / force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def maxOption[T](l: Seq[T])(implicit o: Ordering[T]): Option[T] = if (l.isEmpty) None else Some(l.max(o))

  implicit class MyVec(v1: DVec) {
    def deg360(v2: DVec): Double = {
      val scalar = v1.perpendicular * v2
      if (scalar >= 0) v1.deg(v2) else 360 - v1.deg(v2)
    }

    def rad2Pi(v2: DVec): Double = {
      val scalar = v1.perpendicular * v2
      if (scalar >= 0) v1.rad(v2) else 2 * math.Pi - v1.rad(v2)
    }
  }

  def checkAllConditions(conditions: (() => Boolean)*): Boolean = {
    if (conditions.isEmpty) true
    else if (conditions.head()) checkAllConditions(conditions.tail: _*)
    else false
  }

  implicit class MyDouble(d:Double) {
    def equalPlusMinusOne(x:Double):Boolean = math.abs(d - x) < 1
    def equalPlusMinusTen(x:Double):Boolean = math.abs(d - x) < 10
    def round2Digits:Double = {
      val bd = new java.math.BigDecimal(d)
      bd.setScale(2, java.math.RoundingMode.HALF_UP).doubleValue()
    }

    def myacos:Double = math.acos(math.max(-1, math.min(1, d)))
  }
}
