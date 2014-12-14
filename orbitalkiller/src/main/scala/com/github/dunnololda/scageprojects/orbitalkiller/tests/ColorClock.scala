package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

object ColorClock extends ScageScreenApp("Color Clock", 1280, 1024) {
  private val formatter = new java.text.SimpleDateFormat("yyyy MM dd HH mm ss")
  var time  = formatter.format(new java.util.Date).split(" ")
  var hours = time(3).toInt
  var mins  = time(4).toInt
  var secs  = time(5).toInt

  action(1000) {
    time  = formatter.format(new java.util.Date).split(" ")
    hours = time(3).toInt
    mins  = time(4).toInt
    secs  = time(5).toInt
    backgroundColor = ScageColor("clock", hours/24f, mins/60f, secs/60f)
    //backgroundColor = ScageColor("clock", hours/24f, mins/2f/60f, mins/2f/60f)
  }

  interface {
    print(s"$hours : $mins : $secs", 20, 20, BLACK)
  }

  def finder2(perimeter:Int = 1, sum:BigInt = BigInt(0)):BigInt = {
    val test = (1 to perimeter-2).find {
      case a =>
        val b_c = perimeter-a
        (1 to b_c-1).find {
          case b =>
            val c = b_c-b
            val p1 = b+c-a
            val p2 = a+c-b
            val p3 = a+b-c
            val s = p1*p2*p3
            s > 0 && s % perimeter == 0 && (s/perimeter) % 16 == 0
        }.nonEmpty
    }.nonEmpty
    println(s"$perimeter : $test : $sum")
    if(test) finder2(perimeter+1, sum+perimeter)
    else finder2(perimeter+1, sum)
  }
}
