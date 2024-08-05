package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.factors

import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.factors.PrimeFactors.primeFactors4

import scala.collection.mutable

object Factors {

  // возвращает список делителей данного числа n (простых и непростых)
  private def factors4(n: Long): List[Long] = {
    val res23 = primeFactors4(n)
    (1L :: (1 to res23.length).flatMap(i => res23.combinations(i).map(_.product)).toList).filterNot(_ == n) ::: n :: Nil
  }

  private val facts = mutable.HashMap[Long, List[Long]]()

  // возвращает список делителей, кеширует результат
  def factors5(n: Long): List[Long] = {
    facts.getOrElseUpdate(n, factors4(n))
  }
}
