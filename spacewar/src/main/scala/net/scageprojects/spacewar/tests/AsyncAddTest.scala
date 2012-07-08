package net.scageprojects.spacewar.tests

import concurrent.ops._
import net.scage.ScageApp
import collection.mutable.ArrayBuffer

object AsyncAddTest extends ScageApp {
  val start_moment = System.currentTimeMillis()
  spawn {
    while (System.currentTimeMillis() - start_moment < 5000) {}
    println("me here!")
    stop()
  }
}
