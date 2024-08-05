package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.factors

import scala.annotation.tailrec
import scala.collection.mutable

object PrimeFactors {
  // быстрая функция проверки, является ли число простым.
  // Идея в том, чтобы проверить, делиться ли число n на простые от 2 до
  // корня из числа n. Список простых формируется по мере надобности.
  @tailrec private def isPrime(n: Long, sqrt_n: Long = -1l, cur_p: Long = 1l): Boolean = {
    if (sqrt_n == -1L) isPrime(n, math.sqrt(n).toLong, cur_p)
    else {
      if (cur_p >= sqrt_n) true
      else {
        val new_primes_portion = Erat2(math.min(cur_p * 2, sqrt_n)).dropWhile(_ <= cur_p)
        if (new_primes_portion.isEmpty) true
        else {
          val res = new_primes_portion.forall(n % _ != 0)
          if (res) isPrime(n, sqrt_n, new_primes_portion.last)
          else false
        }
      }
    }
  }

  private val pfacts: mutable.Map[Long, List[Long]] = mutable.HashMap[Long, List[Long]]()

  // возвращает список простых чисел, произведение которых равно данному числу n
  private def primeFactors3(n: Long): List[Long] = {
    def _primeFactors3(_n: Long, facs: List[Long] = Nil): List[Long] = {
      pfacts.get(_n).map(x => x ::: facs).getOrElse(
        Erat2(math.sqrt(_n).toLong).find(_n % _ == 0) match {
          case Some(m) => _primeFactors3(_n / m, m :: facs)
          case None => _n :: facs
        }
      )
    }
    _primeFactors3(n)
  }

  // возвращает список простых чисел, произведение которых равно данному числу n, кеширует результат для ускорения последующих запросов
  def primeFactors4(n: Long): List[Long] = {
    pfacts.getOrElseUpdate(n, primeFactors3(n))
  }
}
