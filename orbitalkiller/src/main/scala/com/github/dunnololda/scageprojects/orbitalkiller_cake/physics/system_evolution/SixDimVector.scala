package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution

class SixDimVector(val a1: Double, val a2: Double, val a3: Double, val a4: Double, val a5: Double, val a6: Double) {
  private val l = List(a1, a2, a3, a4, a5, a6)

  def this(l: Seq[Double]) = this(l.head, l(1), l(2), l(3), l(4), l(5))

  def *(k: Double): SixDimVector = new SixDimVector(l.map(_ * k))

  def *(p: SixDimVector): Double = l.zip(p.l).map(kv => kv._1 * kv._2).sum

  def **(p: SixDimVector): SixDimVector = new SixDimVector(l.zip(p.l).map(kv => kv._1 * kv._2))

  def +(p: SixDimVector): SixDimVector = new SixDimVector(l.zip(p.l).map(kv => kv._1 + kv._2))

  /*val bVel = DVec(a1, a2)
  val bAngVelRad = a3
  val aVel = DVec(a4, a5)
  val aAngVelRad = a6*/
}
