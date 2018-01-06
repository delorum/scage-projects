package com.github.dunnololda.scageprojects.orbitalkiller.util.math

import scala.collection.mutable

/**
  * Created by andrey on 1/6/18.
  */
object Factors {
  // структура хранит и по необходимости довычисляет набор простых чисел. Вычисление производится методом решета Эратосфена
  private object erat2 {
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
}
