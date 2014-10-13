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
    val checked = collection.mutable.HashSet[Int]()
    val result  = collection.mutable.HashMap[Int, (collection.mutable.ListBuffer[Int], Int)](from -> (collection.mutable.ListBuffer[Int](from), 0))

    def _nextNode(node:Int, path_here:collection.mutable.ListBuffer[Int]) {
      val neighbours = network(node).filterNot(x => checked.contains(x._1))
      for {
        neighbour <- neighbours
        path_to_neighbour = path_here += neighbour._1
        neighbour_w = result(node)._2 + neighbour._2
        (cur_neighbour_path, cur_neighbour_w) = result.getOrElseUpdate(neighbour._1, (path_to_neighbour, neighbour_w))
      } {
        if(neighbour_w < cur_neighbour_w) result(neighbour._1) = (path_to_neighbour, neighbour_w)
        checked += node
      }
      neighbours.foreach(x => _nextNode(x._1, path_here += x._1))
    }
    _nextNode(from, collection.mutable.ListBuffer[Int](from))

    result.map(kv => (kv._1, (kv._2._1.toList, kv._2._2))).toMap
  }

  /*
    Алгоритм Дийкстры для нахождения кратчайших путей из начальной точки во все остальные точки на графе
    В данном случае используется граф, у которого все переходы равнозначны - имеют вес 1.
    Узлы графа помечаются номерами Int
    Параметр network описывает граф: мапа узел графа -> список возможных переходов из данного узла
    Возвращает мапу: узел графа -> кратчайший путь до него
  */
  def dijkstra1(from:Int, network:Map[Int, List[Int]]):Map[Int, List[Int]] = {
    val checked = collection.mutable.HashSet[Int]()
    val result  = collection.mutable.HashMap[Int, collection.mutable.ListBuffer[Int]](from -> collection.mutable.ListBuffer[Int](from))

    def _nextNode(node:Int, path_here:collection.mutable.ListBuffer[Int]) {
      val neighbours = network(node).filterNot(x => checked.contains(x))
      for {
        neighbour <- neighbours
        path_to_neighbour = path_here += neighbour
        cur_neighbour_path= result.getOrElseUpdate(neighbour, path_to_neighbour)
      } {
        if(path_to_neighbour.length < cur_neighbour_path.length) result(neighbour) = path_to_neighbour
        checked += node
      }
      neighbours.foreach(x => _nextNode(x, path_here += x))
    }
    _nextNode(from, collection.mutable.ListBuffer[Int](from))

    result.map(kv => (kv._1, kv._2.toList)).toMap
  }
}
