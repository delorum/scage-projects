package su.msk.dunno.scage.tutorials.gravitation

import net.scage.ScreenApp
import org.lwjgl.opengl.GL11
import net.scage.ScageLib._
import net.scage.support.Vec
import net.scage.handlers.controller2.MultiController

object Orbita extends ScreenApp("Orbita") with MultiController {
  val G = property("G", 5)
  val dt = property("dt", 1f)



  val num_stars = property("stars.amount", 100)
  val stars = displayList {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glBegin(GL11.GL_POINTS)
    for(i <- 1 to num_stars) {
      val vec = OrbitalTracer.randomCoord()
      GL11.glVertex2f(vec.x, vec.y)
    }
    GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }
  render {
    drawDisplayList(stars)
  }

  val ship = new SpaceShip(Vec(1,0))
  OrbitalTracer.addTrace(Vec(-500, -500), ship)

  val sun =     new Planet(Vec( 0,  0),                                 property("sun.mass",     100000),   10, YELLOW)
  OrbitalTracer.addTrace(Vec(0, 0), sun)

  val mercury = new Planet(Vec(-1,  0)* property("mercury.speed", 35),  property("mercury.mass", 100),    10, GRAY)
  OrbitalTracer.addTrace(Vec(0, -300), mercury)

  /*val venus =   new Planet(Vec(-1,  2)* property("venus.speed",   25),  property("venus.mass",   1000),    10, CYAN)
  OrbitalTracer.addTrace(Vec(-200, -300), venus)

  val earth =   new Planet(Vec( 1, -2)* property("earth.speed",   25),  property("earth.mass",   1000),    10, BLUE)
  OrbitalTracer.addTrace(Vec(200, 300), earth)

  val mars =    new Planet(Vec(-1,  2)*property("mars.speed",     25),  property("mars.mass",    1000),    10, RED)
  OrbitalTracer.addTrace(Vec(-400, 0), mars)*/

  center = sun.location
  key(KEY_S, onKeyDown = {
    println("no pause. S pressed")
    if(center == ship.location) center = sun.location
    else center = ship.location
  })

  keyIgnorePause(KEY_ADD, 100, onKeyDown = globalScale += 1)
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = if(globalScale > 1) globalScale -= 1)

  keyIgnorePause(KEY_P, onKeyDown = {
    if(onPause) {
      pauseOff()
      TrajectoryTracer.stopCalculating()
    } else {
      pause()
      TrajectoryTracer.calculateTrajectories()
    }
  })

  interface {
    if(!onPause) {
      print("velocity: "     + ship.velocity.norma,            10, windowHeight-20, YELLOW)
      print("acceleration: " + ship.acceleration,              10, windowHeight-40, YELLOW)
      print("time: "         + OrbitalTracer.time,             10, windowHeight-60, YELLOW)
    }
    print(fps, windowWidth-20, windowHeight-20, YELLOW)
  }

  pause()
  TrajectoryTracer.calculateTrajectories()
}














