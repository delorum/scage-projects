package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.AABB

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PlanetPart(val m: Double, var coord: Vec, var vel: Vec) {
  val id = GenesisTest.getId
  val r: Float = 1 /*+m*0.001*/

  def aabb: AABB = AABB(coord, r * 2, r * 2)

  def isCollided(op: PlanetPart): Boolean = coord.dist(op.coord) < r + op.r

  var new_force: Vec = Vec.zero

  override val hashCode: Int = id

  def canEqual(other: Any): Boolean = other.isInstanceOf[PlanetPart]

  override def equals(other: Any): Boolean = other match {
    case that: PlanetPart => (that canEqual this) && this.hashCode == that.hashCode
    case _ => false
  }
}

class MySpace(val bodies: Seq[PlanetPart], val c: Vec, val w: Double, val h: Double) {
  val id = GenesisTest.getId

  def this(bodies: Seq[PlanetPart], c: Vec) = {
    this(bodies, c, {
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
      math.max(math.abs(max_x - c.x) * 2, math.abs(c.x - min_x) * 2)
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
      math.max(math.abs(max_y - c.y) * 2, math.abs(c.y - min_y) * 2)
    })
  }

  lazy val aabb: AABB = AABB(c, w, h)

  lazy val quadSpaces: List[MySpace] = {
    val AABB(c, w, h) = aabb

    val c1 = c.toVec + Vec(-w / 4, -h / 4)
    val aabb1 = AABB(c1, w / 2, h / 2)
    val bodies1 = bodies.filter(b => b.aabb.collision(aabb1))

    val c2 = c.toVec + Vec(-w / 4, h / 4)
    val aabb2 = AABB(c2, w / 2, h / 2)
    val bodies2 = bodies.filter(b => b.aabb.collision(aabb2))

    val c3 = c.toVec + Vec(w / 4, h / 4)
    val aabb3 = AABB(c3, w / 2, h / 2)
    val bodies3 = bodies.filter(b => b.aabb.collision(aabb3))

    val c4 = c.toVec + Vec(w / 4, -h / 4)
    val aabb4 = AABB(c4, w / 2, h / 2)
    val bodies4 = bodies.filter(b => b.aabb.collision(aabb4))

    List(
      new MySpace(bodies1, c1, w / 2, h / 2),
      new MySpace(bodies2, c2, w / 2, h / 2),
      new MySpace(bodies3, c3, w / 2, h / 2),
      new MySpace(bodies4, c4, w / 2, h / 2)
    )
  }

  lazy val virtualCentralBody = new PlanetPart(
    m = bodies.map(_.m).sum,
    coord = bodies.map(_.coord).sum / bodies.length,
    vel = bodies.map(_.vel).sum / bodies.length
  )
}

object GenesisTest extends ScageScreenAppMT("Genesis Test", 800, 600) {
  val G: Double = 1
  val dt = 0.1

  val interacted = mutable.HashSet[(Int, Int)]()

  def setInteracted(id1: Int, id2: Int) {
    if (id1 <= id2) interacted += ((id1, id2))
    else interacted += ((id2, id1))
  }

  def checkInteracted(id1: Int, id2: Int): Boolean = {
    if (id1 <= id2) interacted.contains((id1, id2))
    else interacted.contains((id2, id1))
  }

  implicit class RichVec(v: Vec) {
    def isNaN: Boolean = {
      val ans = v.x.toDouble.isNaN || v.y.toDouble.isNaN
      if (ans) {
        "gotach1"
      }
      ans
    }
  }

  private val to_remove = mutable.ArrayBuffer[mutable.HashSet[PlanetPart]]()
  private val planets = ArrayBuffer[PlanetPart]()

  private var _record_points = false
  private val points = ArrayBuffer[(Vec, PlanetPart)]()

  private def randomCoord: Vec = {
    windowCenter + Vec((math.random * 300 / math.sqrt(2)).toFloat, (math.random * 300 / math.sqrt(2)).toFloat).rotateDeg(math.random * 360)
  }

  /*planets += new Planet(5, windowCenter +Vec(-10,0), Vec.zero)
  planets += new Planet(5, windowCenter +Vec(10,0), Vec.zero)*/

  private var _center = center
  private var _selected_planet = 0

  private var id = 1

  def getId: Int = {
    val x = id
    id += 1
    x
  }

  (1 to 3000).foreach(i => {
    val c = randomCoord
    planets += new PlanetPart(1, c,
      (c - windowCenter).n /*(c - windowCenter).rotateDeg(90).n*/
    )
  })

  /**
   *
   * @param space - начальное пространство, которое будем разделять
   * @param max_level - сколько максимально может быть уровней вложенности пространств
   * @param target - сколько максимально может быть тел внутри пространства
   * @param level - текущий уровень вложенности
   * @param spaces - результат, список пространств
   * @return
   */
  private def splitMySpace(space: MySpace, max_level: Int, target: Int, level: Int = 0, spaces: List[MySpace] = Nil): List[MySpace] = {
    if (space.bodies.length <= target) {
      space :: spaces
    } else if (level > max_level) {
      space :: spaces
    } else {
      space.quadSpaces.flatMap {
        case s => splitMySpace(s, max_level, target, level + 1, spaces)
      }
    }
  }

  keyIgnorePause(KEY_W, 10, onKeyDown = {
    _center = center; _center += Vec(0, 5 / globalScale); center = _center
  })
  keyIgnorePause(KEY_A, 10, onKeyDown = {
    _center = center; _center += Vec(-5 / globalScale, 0); center = _center
  })
  keyIgnorePause(KEY_S, 10, onKeyDown = {
    _center = center; _center += Vec(0, -5 / globalScale); center = _center
  })
  keyIgnorePause(KEY_D, 10, onKeyDown = {
    _center = center; _center += Vec(5 / globalScale, 0); center = _center
  })
  keyIgnorePause(KEY_SPACE, onKeyDown = {
    val sorted_planets = planets.sortBy(-_.m)
    planets.clear()
    planets ++= sorted_planets
    _selected_planet = 0
    val p = planets(_selected_planet)
    center = p.coord
  })
  keyIgnorePause(KEY_LEFT, onKeyDown = {
    _selected_planet -= 1
    if (_selected_planet < 0) _selected_planet = planets.length - 1
    val p = planets(_selected_planet)
    center = p.coord
  })
  keyIgnorePause(KEY_RIGHT, onKeyDown = {
    _selected_planet += 1
    if (_selected_planet >= planets.length) _selected_planet = 0
    val p = planets(_selected_planet)
    center = p.coord
  })
  keyIgnorePause(KEY_Q, onKeyDown = if (keyPressed(KEY_LCONTROL)) stopApp())
  keyIgnorePause(KEY_P, onKeyDown = switchPause())
  keyIgnorePause(KEY_1, onKeyDown = _record_points = !_record_points)
  keyIgnorePause(KEY_2, onKeyDown = points.clear())
  keyIgnorePause(KEY_3, onKeyDown = points --= {
    points.groupBy(_._2).flatMap(kv => kv._2.zipWithIndex.filter(x => x._2 % 2 == 0).map(_._1))
  })

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if (globalScale > 0.01) {
      if (globalScale.toInt > 100000) globalScale -= 100000
      else if (globalScale.toInt > 10000) globalScale -= 10000
      else if (globalScale.toInt > 1000) globalScale -= 1000
      else if (globalScale.toInt > 100) globalScale -= 100
      else if (globalScale.toInt > 10) globalScale -= 10
      else if (globalScale.toInt > 1) globalScale -= 1
      else if ((globalScale * 10).toInt > 1) globalScale -= 0.1f
      else globalScale -= 0.01f
      if (globalScale < 0.01) globalScale = 0.01f
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if (globalScale < 1000000) {
      if (globalScale < 0.1) globalScale += 0.01f
      else if (globalScale < 1) globalScale += 0.1f
      else if (globalScale < 10) globalScale += 1
      else if (globalScale < 100) globalScale += 10
      else if (globalScale < 1000) globalScale += 100
      else if (globalScale < 10000) globalScale += 1000
      else if (globalScale < 100000) globalScale += 10000
      else globalScale += 100000
    }
    println(globalScale)
  })

  val start = System.currentTimeMillis()

  def time = System.currentTimeMillis() - start

  action {
    if (planets.map(_.m).sum < 3000) {
      println("gotcha!")
    }
    if (planets.map(_.m).sum > 3000) {
      println("gotach!")
    }

    println(s"[$time] splitting space...")
    val spaces = splitMySpace(new MySpace(planets, windowCenter), 5, 20)
    println(s"[$time] spaces = ${spaces.length}; ${spaces.map(_.bodies.length).mkString(" : ")}")

    interacted.clear()

    for {
      (space, space_idx) <- spaces.zipWithIndex
      if space.bodies.nonEmpty
    } {
      val other_spaces = spaces.take(space_idx) ++ spaces.drop(space_idx + 1)
      val (outer_spaces, near_spaces) = other_spaces.partition(_.virtualCentralBody.coord.dist(space.virtualCentralBody.coord) > 100)
      val filtered_near_spaces = near_spaces.filterNot(_.id == space.id)
      for {
        (p, idx) <- space.bodies.zipWithIndex
      } {
        val outer_spaces_force = outer_spaces.map {
          case os =>
            val x = os.virtualCentralBody
            val d = p.coord.dist2(x.coord)
            if (d != 0) {
              G * p.m * x.m / d * (x.coord - p.coord).n
            }
            else {
              Vec.zero
            }
        }.sum
        if (outer_spaces_force.isNaN) {
          println("gotach!")
        }
        p.new_force += outer_spaces_force

        filtered_near_spaces.foreach {
          case near_space => near_space.bodies.foreach {
            case near_space_body =>
              if (p.id != near_space_body.id && !checkInteracted(p.id, near_space_body.id)) {
                val force = G * p.m * near_space_body.m / p.coord.dist2(near_space_body.coord) * (near_space_body.coord - p.coord).n
                if (force.isNaN) {
                  println("gotach!")
                }
                p.new_force += force
                near_space_body.new_force -= force
                setInteracted(p.id, near_space_body.id)
              }
          }
        }
        if (idx != space.bodies.length - 1) {
          for {
            op <- space.bodies.drop(idx + 1)
          } {
            if (p.id != op.id && !checkInteracted(p.id, op.id)) {
              if (p.isCollided(op)) {
                val sl = to_remove.filter(s => s.contains(p) || s.contains(op))
                if (sl.nonEmpty) {
                  if (sl.length == 1) {
                    sl.head += p
                    sl.head += op
                  } else {
                    sl.head += p
                    sl.head += op
                    sl.tail.foreach(x => sl.head ++= x)
                    to_remove --= sl.tail
                  }
                  //println(s"${p.id} collides with ${op.id} : ${sl.head.map(_.id).mkString(" ")}")
                } else {
                  val s = mutable.HashSet(p, op)
                  //println(s"${p.id} collides with ${op.id} : ${s.map(_.id).mkString(" ")}")
                  to_remove += s
                }
              } else {
                val force = G * p.m * op.m / p.coord.dist2(op.coord) * (op.coord - p.coord).n
                if (force.isNaN) {
                  println("gotach!")
                }
                p.new_force += force
                op.new_force -= force
              }
              setInteracted(p.id, op.id)
            }
          }
        }
      }
    }

    if (planets.map(_.m).sum < 3000) {
      println("gotcha!")
    }
    if (planets.map(_.m).sum > 3000) {
      println("gotach!")
    }

    println("================================")
    to_remove.foreach {
      case ps =>
        val psl = ps.toSeq
        //println(psl.map(_.id).mkString(" "))
        val newm = psl.map(_.m).sum
        val newp = new PlanetPart(
          newm,
          psl.map(_.coord).sum / ps.size,
          psl.map(op => op.m * op.vel).sum / newm)
        //println(planets.map(_.m).sum)
        //println(ps.forall(p => planets.contains(p)))
        planets --= ps
        //println(planets.map(_.m).sum)
        planets += newp
        //println(planets.map(_.m).sum)
        if (planets.map(_.m).sum < 3000) {
          println("gotcha!")
        }
        if (planets.map(_.m).sum > 3000) {
          println("gotach!")
        }
    }

    to_remove.clear()

    planets.foreach {
      case p =>
        p.vel += p.new_force / p.m * dt
        if (p.vel.isNaN) {
          println("gotach!")
        }
        p.coord += p.vel * dt
        if (p.coord.isNaN) {
          println("gotach!")
        }
        p.new_force = Vec.zero
        if (_record_points) points += ((p.coord - center, p))
    }

    /*for {
      (p, idx) <- planets.zipWithIndex
    } {
      if(idx != planets.length-1) {
        for {
          op <- planets.drop(idx + 1)
        } {
          val force = GenesisTest.G * p.m * op.m / p.coord.dist2(op.coord) * (op.coord - p.coord).n
          p.new_force += force
          op.new_force -= force
        }
      }
      p.vel += p.new_force/p.m*dt
      p.coord += p.vel*dt
      p.new_force = Vec.zero
      if(_record_points) points += ((p.coord - center, p))
    }*/
  }

  center = _center

  render {
    planets.foreach {
      case p =>
        drawCircle(p.coord, p.r.toFloat, WHITE)
        if (globalScale >= 8) {
          print(p.m, p.coord, max_font_size / globalScale, WHITE, "center")
        }
    }

    points.foreach(p => drawFilledCircle(center + p._1, 1, WHITE))
  }

  interface {
    if (onPause) print("Пауза", windowCenter.toVec, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$ticks", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec * fps / (averageRenderTimeMsec * fps + averageActionTimeMsec * ticks) * 100}%.2f%%/${1 * averageActionTimeMsec * ticks / (averageRenderTimeMsec * fps + averageActionTimeMsec * ticks) * 100}%.2f%%", windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec", windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec", windowWidth - 20, windowHeight - 100, align = "top-right", color = DARK_GRAY)
    print(s"planets: ${planets.size}, mass: ${planets.map(_.m).sum}", 20, 20, GRAY)
  }
}
