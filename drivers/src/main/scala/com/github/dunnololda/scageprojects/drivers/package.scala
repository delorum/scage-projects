package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLib._

package object drivers {
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

  implicit class MyVec(v1:Vec) {
    def mydeg(v2:Vec):Float = {
      val scalar = v1*v2.perpendicular
      if(scalar >= 0) v1.deg(v2) else -v1.deg(v2)
    }
  }
}
