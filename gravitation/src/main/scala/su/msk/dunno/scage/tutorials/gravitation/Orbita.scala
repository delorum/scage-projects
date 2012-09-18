package su.msk.dunno.scage.tutorials.gravitation

import net.scage.{ScageScreenApp, ScageScreen}
import org.lwjgl.opengl.GL11
import net.scage.ScageLib._
import net.scage.support.Vec

object Orbita extends ScageScreenApp("Orbita") {
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
  keyNoPause(KEY_S, onKeyDown = {
    if(center == ship.location) center = sun.location
    else center = ship.location
  })

  keyNoPause(KEY_ADD, 100, onKeyDown = globalScale += 1)
  keyNoPause(KEY_SUBTRACT, 100, onKeyDown = if(globalScale > 1) globalScale -= 1)
  keyPause(KEY_LEFT, 50, onKeyDown = {
    ship.rotate(5)
    if(onPause && ship.acceleration.norma2 > 0) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_RIGHT, 50, onKeyDown = {
    ship.rotate(-5)
    if(onPause && ship.acceleration.norma2 > 0) TrajectoryTracer.calculateTrajectories()
  })

  keyPause(KEY_UP, 100, onKeyDown = {
    ship.applyForce(ship.direction.n*0.1f)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_DOWN, 100, onKeyDown = {
    ship.applyForce(ship.direction.n*(-0.1f))
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })

  keyPause(KEY_NUMPAD7, 100, onKeyDown = {
    ship.increaseAccelerationPeriod(1)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_NUMPAD1, 100, onKeyDown = {
    ship.decreaseAccelerationPeriod(1)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_NUMPAD8, 100, onKeyDown = {
    ship.increaseAccelerationPeriod(10)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_NUMPAD2, 100, onKeyDown = {
    ship.decreaseAccelerationPeriod(10)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_NUMPAD9, 100, onKeyDown = {
    ship.increaseAccelerationPeriod(100)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })
  keyPause(KEY_NUMPAD3, 100, onKeyDown = {
    ship.decreaseAccelerationPeriod(100)
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })

  keyPause(KEY_SPACE, onKeyDown = {
    ship.removeAcceleration()
    if(onPause) TrajectoryTracer.calculateTrajectories()
  })

  keyNoPause(KEY_P, onKeyDown = {
    if(onPause) {
      pauseOff()
      TrajectoryTracer.stopCalculating()
      ship.setAccelerationMoment(ship.accelerationPeriod)
    } else {
      pause()
      ship.setAccelerationMoment(0)
      TrajectoryTracer.calculateTrajectories()
    }
  })

  interface {
    print(fps, windowWidth-20, windowHeight-20, YELLOW)
  }

  pause()
  TrajectoryTracer.calculateTrajectories()
}














