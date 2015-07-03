package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{DynamicShape => Phys2dShape, Body => Phys2dBody, StaticBody => Phys2dStaticBody, Collider => Phys2dCollider, BodyList => Phys2dBodyList, _}

import scala.collection.mutable
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

  case class PolygonShape(points:List[DVec]) extends Shape {
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

  class Space(val bodies:List[MutableBodyState], val center:DVec, val width:Double, val height:Double) {
    def this(bodies:List[MutableBodyState], center:DVec) = {
      this(bodies, center, {
        val (init_min_x, init_max_x) = {
          bodies.headOption.map(b => {
            val AABB(c, w, _) = b.body.aabb
            (c.x - w/2, c.x+w/2)
          }).getOrElse((0.0, 0.0))
        }
        val (min_x, max_x) = bodies.foldLeft((init_min_x, init_max_x)) {
          case ((res_min_x, res_max_x), b) =>
            val AABB(c, w, _) = b.body.aabb
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
            val AABB(c, _, h) = b.body.aabb
            (c.y-h/2, c.y+h/2)
          }).getOrElse((0.0, 0.0))
        }
        val (min_y, max_y) = bodies.foldLeft(init_min_y, init_max_y) {
          case ((res_min_y, res_max_y), b) =>
            val AABB(c, _, h) = b.body.aabb
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
      val bodies1 = bodies.filter(b => b.body.aabb.aabbCollision(aabb1))

      val c2 = c + DVec(-w/4, h/4)
      val aabb2 = AABB(c2, w/2, h/2)
      val bodies2 = bodies.filter(b => b.body.aabb.aabbCollision(aabb2))

      val c3 = c + DVec(w/4, h/4)
      val aabb3 = AABB(c3, w/2, h/2)
      val bodies3 = bodies.filter(b => b.body.aabb.aabbCollision(aabb3))

      val c4 = c + DVec(w/4, -h/4)
      val aabb4 = AABB(c4, w/2, h/2)
      val bodies4 = bodies.filter(b => b.body.aabb.aabbCollision(aabb4))

      List(
        new Space(bodies1, c1, w/2, h/2),
        new Space(bodies2, c2, w/2, h/2),
        new Space(bodies3, c3, w/2, h/2),
        new Space(bodies4, c4, w/2, h/2)
      )
    }
  }

  // http://en.wikipedia.org/wiki/Quadtree
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
    def applyImpulse(_dt:Double) {
      val a_prev_vel = a.vel
      val b_prev_vel = b.vel

      val e = /*1*//*0.9*/math.min(a.body.restitution, b.body.restitution)

      // radii from centers of masses to contact
      val ra = contact_point - a.coord
      val rb = contact_point - b.coord

      val rv = b.vel + (b.ang_vel.toRad */ rb) - a.vel - (a.ang_vel.toRad */ ra)  // Relative velocity

      val contactVel = rv*normal // Relative velocity along the normal
      if(contactVel <= 0) { // Do not resolve if velocities are separating
        val raCrossN = ra */ normal
        val rbCrossN = rb */ normal

        val invMassSum = a.body.invMass + b.body.invMass + (raCrossN*raCrossN)*a.body.invI + (rbCrossN*rbCrossN)*b.body.invI
        val  j = (-(1.0f + e) * contactVel)/invMassSum///contact_points.length
        val impulse = normal * j
        a.applyImpulse(-impulse, ra)
        b.applyImpulse(impulse, rb)

        val t = (rv + normal*(-rv*normal)).n
        val jt = (-(rv*t))/invMassSum///contact_points.length
        if(math.abs(jt) > 0.0001) {
          // Coulumb's law
          val tangentImpulse = if(math.abs(jt) < j*a.body.staticFriction) {
            t * jt
          } else {
            t*j*(-a.body.dynamicFriction)
          }
          a.applyImpulse(-tangentImpulse, ra)
          b.applyImpulse(tangentImpulse, rb)
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
      val correction = math.max(separation - 0.05, 0)/(a.body.invMass + b.body.invMass)*0.4
      if(!a.body.is_static) a.coord += normal*(-a.body.invMass*correction)
      if(!b.body.is_static) b.coord += normal*b.body.invMass*correction
    }

    def toImmutable = Contact(a.toImmutableBodyState,
                              b.toImmutableBodyState,
                              contact_point,
                              normal,
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

  def maybeCollision(body1:MutableBodyState, body2:MutableBodyState):Option[MutableContact] = {
    def collide(pb1:Phys2dBody, pb2:Phys2dBody, collider:Phys2dCollider):Option[GeometricContactData] = {
      val num_contacts = collider.collide(contacts, pb1, pb2)
      if(num_contacts == 0) None
      else {
        num_contacts match {
          case 1 =>
            val contact_point = contacts(0).getPosition.toDVec
            val normal = contacts(0).getNormal.toDVec
            Some(GeometricContactData(contact_point, normal, math.abs(contacts(0).getSeparation)))
          case 2 =>
            val contact_points = List(contacts(0).getPosition.toDVec, contacts(1).getPosition.toDVec)
            val contact_point = contact_points.sum/contact_points.length
            val normal = contacts(0).getNormal.toDVec
            val separation = math.max(math.abs(contacts(0).getSeparation), math.abs(contacts(1).getSeparation))
            Some(GeometricContactData(contact_point, normal, separation))
          case _ => None
        }
      }
    }
    body1.body.shape match {
      case c1:CircleShape =>
        body2.body.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, circle_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case l2:LineShape =>
            collide(body2.phys2dBody, body1.phys2dBody, line_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, circle_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case p2:PolygonShape =>
            collide(body2.phys2dBody, body1.phys2dBody, polygon_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
          case _ => None
        }
      case l1:LineShape =>
        body2.body.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_circle_collider).map(gcd => MutableContact(body2, body1, gcd.contact_point, gcd.normal, gcd.separation))
          case l2:LineShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_line_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case p2:PolygonShape =>
            collide(body1.phys2dBody, body2.phys2dBody, line_polygon_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case _ => None
        }
      case b1:BoxShape =>
        body2.body.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, box_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case l2:LineShape =>
            collide(body2.phys2dBody, body1.phys2dBody, line_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, box_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case p2:PolygonShape =>
            collide(body2.phys2dBody, body1.phys2dBody, polygon_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
          case _ => None
        }
      case p1:PolygonShape =>
        body2.body.shape match {
          case c2:CircleShape =>
            collide(body1.phys2dBody, body2.phys2dBody, polygon_circle_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case l2:LineShape =>
            collide(body2.phys2dBody, body1.phys2dBody, line_polygon_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, -gcd.normal, gcd.separation))
          case b2:BoxShape =>
            collide(body1.phys2dBody, body2.phys2dBody, polygon_box_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case p2:PolygonShape =>
            collide(body1.phys2dBody, body2.phys2dBody, polygon_polygon_collider).map(gcd => MutableContact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
          case _ => None
        }
      case _ => None
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
                       dynamicFriction:Double = 0.3) {
    val aabb = shape.aabb(coord, ang)
    val I = mass*shape.wI
    val invMass = if(is_static || mass == 0) 0 else 1.0/mass
    val invI = if(is_static || I == 0) 0 else 1.0/I

    def toMutableBodyState = new MutableBodyState(this)
  }

  class MutableBodyState(val body:BodyState) {
    var vel:DVec = body.vel
    var coord:DVec = body.coord

    var ang_vel:Double = body.ang_vel
    var ang:Double = body.ang

    def aabb = body.shape.aabb(coord, ang)

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

    def applyImpulse(impulse:DVec, contactVector:DVec) {
      if(!body.is_static) {
        vel += impulse * body.invMass
        ang_vel += (body.invI * (contactVector */ impulse)).toDeg
      }
    }
    
    def toImmutableBodyState:BodyState = body.copy(coord = coord, vel = vel, ang_vel = ang_vel, ang = ang)
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
        space <- splitSpace(new Space(mutable_bodies, DVec.zero), 5, 10)
        if space.bodies.length > 1
        (b1, idx) <- space.bodies.zipWithIndex.init
        b2 <- space.bodies.drop(idx+1)
        if !b1.body.is_static || !b2.body.is_static
        c <- maybeCollision(b1, b2)
      } yield c} else Nil
      
      val mb_and_others = for {
        (mb, idx) <- mutable_bodies.zipWithIndex
        other_mutable_bodies = mutable_bodies.take(idx) ::: mutable_bodies.drop(idx + 1)
      } yield (mb, other_mutable_bodies)
      
      // Integrate forces first part
      mb_and_others.foreach {case (mb, other_mutable_bodies) =>
        if(!mb.body.is_static) {
          val b = mb.toImmutableBodyState
          val other_bodies = other_mutable_bodies.map(_.toImmutableBodyState)
          val next_force = force(tacts, b, other_bodies)
          val next_acc = next_force * mb.body.invMass
          val next_torque = torque(tacts, b, other_bodies)
          val next_ang_acc = (next_torque * mb.body.invI).toDeg // in degrees
          mb.vel += next_acc * _dt * 0.5
          mb.ang_vel += next_ang_acc * _dt * 0.5
        }
      }

      // Solve collisions
      if(collisions.nonEmpty) collisions.foreach(c => c.applyImpulse(_dt))

      // Integrate velocities and forces last part
      mb_and_others.foreach{ case (mb, other_mutable_bodies) =>
        if(!mb.body.is_static) {
          val b = mb.toImmutableBodyState
          mb.coord += mb.vel*_dt
          mb.ang = correctAngle((mb.ang + mb.ang_vel*_dt) % 360)
          val other_bodies = other_mutable_bodies.map(_.toImmutableBodyState)
          val next_force = force(tacts, b, other_bodies)
          val next_acc = next_force * b.invMass
          val next_torque = torque(tacts, b, other_bodies)
          val next_ang_acc = (next_torque * b.invI).toDeg // in degrees
          mb.vel += next_acc * _dt * 0.5
          mb.ang_vel += next_ang_acc * _dt * 0.5
        }
      }
      
      // Correct positions
      if(collisions.nonEmpty) collisions.foreach(c => c.positionalCorrection())

      val next_bodies = mutable_bodies.map(_.toImmutableBodyState)

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

  def satelliteSpeed(body_coord:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double, counterclockwise:Boolean):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    if(!counterclockwise) planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*(-1)
    else planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)
  }

  def escapeVelocity(body_coord:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double, counterclockwise:Boolean):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    if(!counterclockwise) planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*math.sqrt(2)*(-1)
    else planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*math.sqrt(2)
  }

  def satelliteSpeed(body_coord:DVec, body_velocity:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = math.signum(from_planet_to_body.signedDeg(body_velocity)) > 0
    if(!counterclockwise) planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*(-1)
    else planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)
  }

  def escapeVelocity(body_coord:DVec, body_velocity:DVec, planet_coord:DVec, planet_velocity:DVec, planet_mass:Double, G:Double):DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = math.signum(from_planet_to_body.signedDeg(body_velocity)) > 0
    if(!counterclockwise) planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*math.sqrt(2)*(-1)
    else planet_velocity + from_planet_to_body.p*math.sqrt(G*planet_mass/from_planet_to_body.norma)*math.sqrt(2)
  }

  def timeStr(time_msec:Long):String = {
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
    if(is_below_zero) s"-$result" else result
  }

  def mOrKm(meters:Number):String = {
    if(math.abs(meters.floatValue()) < 1000) f"${meters.floatValue()}%.2f м" else f"${meters.floatValue()/1000}%.2f км"
  }

  def msecOrKmsec(msec:Number):String = {
    if(math.abs(msec.floatValue()) < 1000) f"${msec.floatValue()}%.2f м/сек" else f"${msec.floatValue()/1000}%.2f км/сек"
  }

  /**
   * Ускорение
   * @param msec - метры в секунду
   * @return
   */
  def msec2OrKmsec2(msec:Number):String = {
    if(math.abs(msec.floatValue()) < 1000) f"${msec.floatValue()}%.2f м/сек^2" else f"${msec.floatValue()/1000}%.2f км/сек^2"
  }
  
  sealed trait KeplerOrbit {
    def a:Double
    def b:Double
    def e:Double
    def f:DVec
    def center:DVec
    def strDefinition(prefix:String, planet_radius:Double):String
  }

  case class EllipseOrbit(
    a:Double,             // большая полуось
    b:Double,             // малая полуось
    e:Double,             // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
    c:Double,             // фокальное расстояния (полурасстояние между фокусами)
    p:Double,             // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)
    r_p:Double,           // перигей
    r_a:Double,           // апогей
    t:Double,             // орбитальный период, в секундах
    f:DVec,              // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
    f2:DVec,              // координаты второго фокуса
    center:DVec) extends KeplerOrbit {        // координаты центра
    def strDefinition(prefix:String, planet_radius:Double):String = f"$prefix, замкнутая, e = $e%.2f, r_p = ${mOrKm(r_p - planet_radius)}, r_a = ${mOrKm(r_a - planet_radius)}, t = ${timeStr((t*1000l).toLong)}"
  }          

  case class HyperbolaOrbit(
     a:Double,             // большая полуось
     b:Double,
     e:Double,             // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
     f:DVec,                            // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
     center:DVec) extends KeplerOrbit { // координаты центр
  def strDefinition(prefix:String, planet_radius:Double):String = f"$prefix, незамкнутая, e = $e%.2f, r_p = ${mOrKm(a*(e-1) - planet_radius)}"
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

      EllipseOrbit(a, b, e, c, p, r_p, r_a, t, f1, f2, center)
    } else {
      val e = math.sqrt(math.abs(1 + (b * b) / (a * a))) // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)

      val true_anomaly = math.abs(math.acos((a*(e*e-1) - body_relative_coord.norma)/(body_relative_coord.norma*e)))

      val counterclockwise = body_relative_coord.signedDeg(body_relative_velocity) > 0
      val moving_away = body_relative_velocity*body_relative_coord.n > 0

      val signum = if(counterclockwise) {
        if(moving_away) -1 else 1
      } else {
        if(moving_away) 1 else -1
      }

      val center = planet_coord + body_relative_coord.rotateRad(true_anomaly*signum).n*a*e

      HyperbolaOrbit(a,b,e,planet_coord, center)
    }
  }

  def calculateHyperbolaOrbit(planet_mass:Double, planet_coord:DVec, body_mass:Double, body_relative_coord:DVec, body_relative_velocity:DVec, G:Double):HyperbolaOrbit = {
    //https://ru.wikipedia.org/wiki/Гравитационный_параметр
    val mu = (planet_mass + body_mass) * G // гравитационный параметр
    //val mu = planet_mass*G // гравитационный параметр

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
    val e = math.sqrt(math.abs(1 + (b * b) / (a * a))) // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)

    val true_anomaly = math.abs(math.acos((a*(e*e-1) - body_relative_coord.norma)/(body_relative_coord.norma*e)))*math.signum(-body_relative_coord.signedDeg(body_relative_velocity))
    val center = planet_coord + body_relative_coord.rotateRad(true_anomaly).n*a*e

    HyperbolaOrbit(a,b,e,planet_coord, center)
  }

  def equalGravityRadius(planet1:BodyState, planet2:BodyState):Double = {
    val A = planet1.coord.dist(planet2.coord)
    val X = planet1.mass/planet2.mass
    A*math.sqrt(X)/(math.sqrt(X) + 1)
  }

  def soi(smaller_planet_mass:Double, semi_major_axis:Double, bigger_planet_mass:Double):Double = {
    semi_major_axis*math.pow(smaller_planet_mass/bigger_planet_mass, 2.0/5)
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
    def mydeg(v2:DVec):Double = {
      val scalar = v1*v2.perpendicular
      if(scalar >= 0) v1.deg(v2) else 360 - v1.deg(v2)
    }
  }
}
