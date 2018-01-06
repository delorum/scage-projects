package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable

object PE233 extends ScageScreenApp("PE233", 640, 480) {
  private def ints(n: Int): Array[Int] = {
    val min = (n * (1.0 - math.sqrt(2)) / 2.0).toInt
    val max = (n * (1.0 + math.sqrt(2)) / 2.0).toInt
    (min to max).toArray
  }

  private def opposite(xy: (Int, Int), n: Int): Set[(Int, Int)] = {
    val (x, y) = xy
    Set(xy, (n - x, y), (x, n - y), (n - x, n - y))
  }

  private def primitive(n: Int): Set[List[(Int, Int)]] = {
    val a = ints(n)
    val ps = (for {
      (x, id) <- a.zipWithIndex
      xx = (x - 1.0 * n / 2.0) * (x - 1.0 * n / 2.0)
      y <- a.drop(id)
      yy = (y - 1.0 * n / 2.0) * (y - 1.0 * n / 2.0)
      if xx + yy == n * n / 2.0
    } yield {
        if (x != y) List((x, y), (y, x)) else List((x, y))
      }).toList.flatten
    val res = mutable.HashSet[Set[(Int, Int)]]()
    ps.foreach {
      case xy@(x, y) =>
        res += opposite(xy, n)
        res += opposite((y, x), n)
    }
    res.map(s => {
      val s1 = s.toArray.sortBy(_._1)
      val one = s1(0)
      val two = s1(1)
      val three = if (s1(2)._2 == two._2) s1(2) else s1(3)
      val four = if (three == s1(2)) s1(3) else s1(2)
      one :: two :: three :: four :: one :: Nil
    }).toSet
  }

  var n: Int = 1000
  var radius: Float = 0f
  var s: Set[List[(Int, Int)]] = Set.empty
  var length: Int = 0
  var result: String = ""
  reCalc()

  def reCalc() {
    radius = (n * math.sqrt(2) / 2).toFloat
    s = primitive(n)
    length = s.toSeq.map(x => x.length - 1).sum
    result = s.toSeq.flatten.forall {
      case (x, y) =>
        val xx = (x - 1.0 * n / 2.0) * (x - 1.0 * n / 2.0)
        val yy = (y - 1.0 * n / 2.0) * (y - 1.0 * n / 2.0)
        xx + yy == n * n / 2.0
    }.toString
  }

  private var _center = Vec(n / 2f, n / 2f)

  key(KEY_UP, 100, onKeyDown = {
    n += 1
    reCalc()
  })
  key(KEY_DOWN, 100, onKeyDown = {
    if (n > 1) {
      n -= 1
      reCalc()
    }
  })

  keyIgnorePause(KEY_W, 10, onKeyDown = {
    _center += Vec(0, 5 / globalScale)
  })
  keyIgnorePause(KEY_A, 10, onKeyDown = {
    _center += Vec(-5 / globalScale, 0)
  })
  keyIgnorePause(KEY_S, 10, onKeyDown = {
    _center += Vec(0, -5 / globalScale)
  })
  keyIgnorePause(KEY_D, 10, onKeyDown = {
    _center += Vec(5 / globalScale, 0)
  })

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if (globalScale > 0.01f) {
      if (globalScale > 1) globalScale -= 1
      else if (globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
      if (globalScale < 0.01f) globalScale = 0.01f
    }
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if (globalScale < 5) {
      if (globalScale < 1) globalScale += 0.1f
      else globalScale += 1
    }
  })

  center = _center
  render {
    drawCircle(Vec(n / 2f, n / 2f), radius, WHITE)
    s.foreach(ss => {
      if (ss.contains((n, n))) drawSlidingLines(ss.map(xy => Vec(xy._1, xy._2)), RED)
      else drawSlidingLines(ss.map(xy => Vec(xy._1, xy._2)), WHITE)
    })
    //drawSlidingLines(s.head.map(xy => Vec(xy._1, xy._2)), WHITE)
  }

  interface {
    print(s"N = $n, length = $length, result = $result", 20, 20, WHITE)
  }

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

  def primeFactors3(n: Long): List[Long] = {
    def _primeFactors3(_n: Long, facs: List[Long]): List[Long] = {
      erat2(math.sqrt(_n).toLong).find(_n % _ == 0) match {
        case Some(m) => _primeFactors3(_n / m, m :: facs)
        case None => _n :: facs
      }
    }
    _primeFactors3(n, List[Long]())
  }

  val pfacts = collection.mutable.HashMap[Long, List[Long]]()

  def factorization1(n: Long): List[Long] = {
    pfacts.getOrElseUpdate(n, primeFactors3(n))
  }

  def factors4(n: Long) = {
    val res23 = factorization1(n)
    (1L :: (1 to res23.length).flatMap(i => res23.combinations(i).map(_.product)).toList).filterNot(_ == n) ::: n :: Nil
  }

  def gcd(numbers: Long*): Long = {
    if (numbers.isEmpty) 1
    else {
      val x = numbers.map(factors4)
      val fs = x.tail.foldLeft(x.head) {
        case (res, f) => res.intersect(f)
      }
      if (fs.isEmpty) 1
      else fs.max
    }
  }

  /*def mnk(before:Int):(Int, Int, Int) = {
    for {
      m <- 1 to before
      n <- 1 to m
      k <- math.sqrt(1.0*m*m*n/(2*m*n)).toInt until math.sqrt(m*n).toInt
      if gcd(m,n,k) == 1
    } yield (m,n,k)
  }*/
}
