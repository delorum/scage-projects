package su.msk.dunno.scage.tutorials.life.tests

class C1 {outer =>
  def pew() {
    println("C1")
  }

  class C2 {
    def pew() {
      outer.pew()
      println("C2")
    }
  }

  val c2 = new C2
}

object OuterTest extends App {
  val c1 = new C1
  c1.c2.pew()
}
