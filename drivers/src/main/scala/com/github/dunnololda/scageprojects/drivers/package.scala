package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._
import net.phys2d.raw.shapes.{DynamicShape => Phys2dShape, _}
import net.phys2d.raw.collide.{Collider => Phys2dCollider, _}
import net.phys2d.raw.{Body => Phys2dBody, StaticBody => Phys2dStaticBody, BodyList => Phys2dBodyList, World => Phys2dWorld}

import scala.collection.mutable

package object drivers {
  val def_vector = Vec(0,1)
  def drawDashedLine(from:Vec, to:Vec, dash_len:Float, color:ScageColor): Unit = {
    val line_len = (to - from).norma
    val normal = (to - from).n
    (0f to line_len-dash_len by dash_len*2).foreach {
      case dash_from => drawLine(from + normal*dash_from, from+normal*(dash_from + dash_len), color)
    }
  }

  /*
    Алгоритм Дийкстры для нахождения кратчайших путей из начальной точки во все остальные точки на взвешенном графе
    Взвешенный граф - у которого заданы числовые веса переходов
    Узлы графа помечаются номерами Int
    Параметр network описывает граф: мапа узел графа -> список возможных переходов из данного узла и их стоимость
    Возвращает мапу: узел графа -> (кратчайший путь до него, стоимость пути)
  */
  def dijkstra(from:Int, network:Map[Int, List[(Int, Int)]]):Map[Int, (List[Int], Int)] = {
    val result  = collection.mutable.HashMap[Int, (List[Int], Int)](from -> (List(from), 0))

    def _nextNode(node:Int, path_here:List[Int], checked:Set[Int]) {
      val neighbours = network(node).filterNot(x => checked.contains(x._1))
      for {
        neighbour <- neighbours
        path_to_neighbour = path_here ::: List(neighbour._1)
        neighbour_w = result(node)._2 + neighbour._2
        (cur_neighbour_path, cur_neighbour_w) = result.getOrElseUpdate(neighbour._1, (path_to_neighbour, neighbour_w))
      } {
        if(neighbour_w < cur_neighbour_w) result(neighbour._1) = (path_to_neighbour, neighbour_w)
      }
      neighbours.foreach(x => _nextNode(x._1, path_here ::: List(x._1), checked + node))
    }
    _nextNode(from, List(from), Set())

    result.map(kv => (kv._1, (kv._2._1.toList, kv._2._2))).toMap
  }

  /*
    Алгоритм Дийкстры для нахождения кратчайших путей из начальной точки во все остальные точки на графе
    В данном случае используется граф, у которого все переходы равнозначны - имеют вес 1.
    Узлы графа помечаются номерами Int
    Параметр network описывает граф: мапа узел графа -> список возможных переходов из данного узла
    Возвращает мапу: узел графа -> кратчайший путь до него
  */
  def dijkstra1(from:Vec, network:Map[Vec, List[Vec]]):Map[Vec, List[Vec]] = {
    val result  = collection.mutable.HashMap[Vec, List[Vec]](from -> List(from))

    def _nextNode(node:Vec, path_here:List[Vec], checked:Set[Vec]) {
      val neighbours = network(node).filterNot(x => checked.contains(x))
      for {
        neighbour <- neighbours
        path_to_neighbour = path_here ::: List(neighbour)
        cur_neighbour_path= result.getOrElseUpdate(neighbour, path_to_neighbour)
      } {
        if(path_to_neighbour.length < cur_neighbour_path.length) result(neighbour) = path_to_neighbour
      }
      neighbours.foreach(x => _nextNode(x, path_here ::: List(x), checked + node))
    }
    _nextNode(from, List(from), Set())

    result.map(kv => (kv._1, kv._2.toList)).toMap
  }

  def aStar(from:Vec, to:Vec, network:Map[Vec, List[Vec]]):List[Vec] = {
    case class VecData(v:Vec, g:Int, h:Float, came_from:Option[VecData]) {
      def f = g + h
    }

    def reconstructPath(goal_node:VecData, res:List[VecData] = Nil):List[VecData] = {
      if(goal_node.came_from.isEmpty) goal_node :: res
      else {
        reconstructPath(goal_node.came_from.get, goal_node :: res)
      }
    }

    val closed_set = mutable.HashMap[Vec, VecData]()
    val open_set = mutable.HashMap[Vec, VecData](from -> VecData(from, g = 0, h = from.dist(to), came_from = None))
    while(open_set.nonEmpty) {
      val x = open_set.toList.sortBy(_._2.f).head
      if(x._1 == to) {
        return reconstructPath(x._2).map(_.v)
      } else {
        open_set -= x._1
        closed_set += x
        for {
          y <- network(x._1)
          if !closed_set.contains(y)
        } {
          val tentative_g_score = x._2.g + 1
          if(!open_set.contains(y) || tentative_g_score < open_set(y).g) {
            open_set(y) = VecData(y, tentative_g_score, y.dist(to), Some(x._2))
          }
        }
      }
    }
    Nil
  }

  case class AABB(center:Vec, width:Double, height:Double) {
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
    def aabb(center:Vec, rotation:Double):AABB
    def phys2dShape:Phys2dShape
    def wI:Double
  }

  case class BoxShape(width:Double, height:Double) extends Shape {
    def aabb(center:Vec, rotation:Double): AABB = {
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

    def phys2dShape: Phys2dShape = new Box(width.toFloat, height.toFloat)

    lazy val wI: Double = (width*width + height*height)/12.0
  }

  private val contacts = Array.fill(10)(new net.phys2d.raw.Contact)
  private val box_box_collider = new BoxBoxCollider

  case class GeometricContactData(contact_point:Vec, normal:Vec, separation:Float)
  case class Contact(body1:BodyState, body2:BodyState, contact_point:Vec, normal:Vec, separation:Float)

  case class BodyState(index:String,
                       mass:Double,
                       acc:Vec,
                       vel:Vec,
                       coord:Vec,
                       ang_acc:Double,
                       ang_vel:Double,
                       ang:Double,
                       shape: Shape,
                       is_static:Boolean) {
    def phys2dBody:Phys2dBody = {
      if(is_static) {
        val b = new Phys2dStaticBody(index, shape.phys2dShape)
        b.setPosition(coord.x.toFloat, coord.y.toFloat)
        b.setRotation(ang.toRad.toFloat)
        b.setUserData((index, shape))
        b
      } else {
        val b = new Phys2dBody(index, shape.phys2dShape, mass.toFloat)
        b.setPosition(coord.x.toFloat, coord.y.toFloat)
        b.setRotation(ang.toRad.toFloat)
        b.adjustVelocity(vel.toPhys2dVec)
        b.adjustAngularVelocity(ang_vel.toRad.toFloat)
        b.setUserData((index, shape))
        b
      }
    }

    def aabb = shape.aabb(coord, ang)

    lazy val I = mass*shape.wI
  }

  def maybeCollision(body1:BodyState, body2:BodyState):Option[Contact] = {
     def collide(pb1:Phys2dBody, pb2:Phys2dBody, collider:Phys2dCollider):Option[GeometricContactData] = {
      val num_contacts = collider.collide(contacts, pb1, pb2)
      if(num_contacts == 0) None
      else {
        num_contacts match {
          case 1 =>
            val contact_point = contacts(0).getPosition.toVec
            val normal = contacts(0).getNormal.toVec
            val separation = contacts(0).getSeparation
            Some(GeometricContactData(contact_point, normal, separation))
          case 2 =>
            val contact_point = (contacts(0).getPosition.toVec + contacts(1).getPosition.toVec)/2
            val normal = contacts(0).getNormal.toVec
            val separation = math.max(contacts(0).getSeparation, contacts(1).getSeparation)
            Some(GeometricContactData(contact_point, normal, separation))
          case _ => None
        }
      }
    }
    collide(body1.phys2dBody, body2.phys2dBody, box_box_collider).map(gcd => Contact(body1, body2, gcd.contact_point, gcd.normal, gcd.separation))
  }
}
