package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._


object HyperbolaOrbitPositionTest extends ScageScreenAppD("Hyperbola Orbit Position Test", 640, 480) {
  val G:Double = 6.6742867E-11
  val moon_mass = 7.3477E22
  val mu = G*moon_mass

  val h = new HyperbolaOrbit(
    a = 9419249.750987718,
    b = 5984119.673034271,
    e = 1.1847428302979859,
    f = DVec(-2.690995073911825E8, 2.6890045578582215E8),
    center = DVec(-2.699968676484078E8, 2.8002370615167433E8),
    mu
  )


  
  val center_minus_f = h.center - h.f
  val inv_n = h.a*math.sqrt(h.a/mu)

  var mr1 = h.f + center_minus_f.n*h.r_p
  var mr2 = h.f + center_minus_f.n*h.r_p

  def tetaDeg360ByDir(dir:DVec):Double = {
    center_minus_f.deg360(dir)
  }

  def tetaDeg360InPoint(p:DVec) = tetaDeg360ByDir(p - h.f)

  def tetaRad2PiByDir(dir:DVec):Double = {
    center_minus_f.rad2Pi(dir)
  }

  def tetaSignedRadByDir(dir:DVec) = center_minus_f.signedRad(dir)

  def tetaSignedRadInPoint(p:DVec) = tetaSignedRadByDir(p - h.f)

  def distanceByDir(dir:DVec):Double = {
    h.p/(1 + h.e*math.cos(tetaRad2PiByDir(dir)))
  }

  def distanceInPoint(point:DVec) = {
    h.p/(1 + h.e*math.cos(tetaSignedRadInPoint(point)))
  }

  def orbitalPointByDir(dir:DVec) = {
    h.f + dir.n*distanceByDir(dir)
  }

  def travelTimeOnOrbitMsecCCW(point1:DVec, point2:DVec):Long = {
    val t1 = tetaDeg360InPoint(point1)
    val t2 = tetaDeg360InPoint(point2)
    val orbital_point1 = h.f + (point1 - h.f).n*distanceInPoint(point1)
    val orbital_point2 = h.f + (point2 - h.f).n*distanceInPoint(point2)
    val r1 = (orbital_point1 - h.f).norma
    val r2 = (orbital_point2 - h.f).norma
    val s = orbital_point1.dist(orbital_point2)

    val chl1 = 1 + (r1 + r2 + s)/(2*h.a)
    val chl2 = 1 + (r1 + r2 - s)/(2*h.a)

    val l1 = math.log(chl1 + math.sqrt(chl1*chl1 - 1))
    val l2 = math.log(chl2 + math.sqrt(chl2*chl2 - 1))
    (inv_n*((math.sinh(l1) - l1) - (math.sinh(l2) - l2))).toLong*1000
  }

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  mouseMotion(onMotion = m => {
    mr2 = orbitalPointByDir(absCoord(m)/scale - h.f)
  })

  val scale = 10e-6
  center = h.f*scale - DVec(0, 200)

  render {
    drawLine(h.f*scale, absCoord(mouseCoord), WHITE)

    val axis = (h.center - h.f).n
    val yy = (-math.acos(-1.0/h.e)+0.1 to math.acos(-1.0/h.e)-0.1 by 0.1).map(true_anomaly => {
      val r = h.a*(h.e*h.e-1)/(1 + h.e*math.cos(true_anomaly))
      (h.f + (axis*r).rotateRad(true_anomaly))*scale
    }).toList
    drawSlidingLines(yy, WHITE)

    drawFilledCircle(mr1*scale, 3, RED)
    drawFilledCircle(mr2*scale, 3, RED)
    drawLine(mr1*scale, mr2*scale, WHITE)
  }

  interface {
    val t = tetaDeg360ByDir(absCoord(mouseCoord)/scale - h.f)
    if(t <= h.teta_deg_min) {
      print(f"teta = 0/$t%.2f/${h.teta_deg_min}%.2f", 20, 40, WHITE)
    } else if(t >= h.teta_deg_max) {
      print(f"teta = ${h.teta_deg_max}%.2f/$t%.2f/360", 20, 40, WHITE)
    } else {
      print(f"teta = invalid angle $t%.2f", 20, 40, WHITE)
    }
    print(f"d = ${distanceByDir(absCoord(mouseCoord)/scale)}%.2f m", 20, 20, WHITE)
  }
}
