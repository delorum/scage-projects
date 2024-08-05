package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{BoxBoxCollider, BoxCircleCollider, CircleBoxCollider, CircleCircleCollider, LineBoxCollider, LineCircleCollider, LineLineCollider, LinePolygonCollider, PolygonBoxCollider, PolygonCircleCollider, PolygonPolygonCollider, Body => Phys2dBody, Collider => Phys2dCollider}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.{BoxShape, CircleShape, LineShape, PolygonShape}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.contacts.{GeometricContactData, MutableContact}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState

object Collider {
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

  def findCollisions(body1: MutableBodyState, body2: MutableBodyState): List[MutableContact] = {
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
            case _: CircleShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, circle_circle_collider).map(gcd => {
                MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation)
              })
            case _: LineShape =>
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
        case _: LineShape =>
          body2.shape match {
            case _: CircleShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, line_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case _: LineShape =>
              _collide(body1.phys2dBody, body2.phys2dBody, line_line_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
            case _: BoxShape =>
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
            case _: LineShape =>
              _collide(body2.phys2dBody, body1.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
            case _: BoxShape =>
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
            case _: LineShape =>
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
}
