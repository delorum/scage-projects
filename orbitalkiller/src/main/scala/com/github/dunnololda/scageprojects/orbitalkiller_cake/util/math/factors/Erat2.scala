package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.factors

import scala.annotation.tailrec

// структура хранит и по необходимости довычисляет набор простых чисел. Вычисление производится методом решета Эратосфена
object Erat2 {
  private var eratl = (2L, Seq[Long](2))

  def apply(n: Long): Seq[Long] = {
    @tailrec
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

  def clear(): Unit = {
    eratl = (2L, Seq[Long](2))
    println("erased primes")
  }
}
