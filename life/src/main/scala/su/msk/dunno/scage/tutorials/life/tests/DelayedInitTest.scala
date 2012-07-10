package su.msk.dunno.scage.tutorials.life.tests

class Pew extends App {
  val arr = "5"
  println("rrr")
}

trait DelayedInitTest {
  def pew:Pew
}

object Grrr extends DelayedInitTest with App {
  val pew = new Pew
  println(pew.arr)
  pew.main(Array[String]())
  println(pew.arr)
}
