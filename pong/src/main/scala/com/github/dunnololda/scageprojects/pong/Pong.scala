package com.github.dunnololda.scageprojects.pong

import com.github.dunnololda.scage.ScageLib._

object Pong extends ScageScreenApp("Pong", 800, 600) {
  val physics = ScagePhysics()

  val right_edge = new StaticLine(Vec(0,0), Vec(0, windowHeight), true)
  val up_edge    = new StaticLine(Vec(0, windowHeight), Vec(windowWidth, windowHeight), true)
  val left_edge  = new StaticLine(Vec(windowWidth, windowHeight), Vec(windowWidth, 0), true)
  val down_edge  = new StaticLine(Vec(windowWidth, 0), Vec(0, 0), true)
  physics.addPhysicals(right_edge, up_edge, left_edge, down_edge)

  var count = 0
  init {
    count = 0
  }

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp())

  val paddle1 = new Paddle(Vec(100-25, windowHeight/2))
  val paddle2 = new Paddle(Vec(windowWidth-100-25, windowHeight/2))

  mouseMotion(onMotion = {m =>
    val y_coord = math.min(math.max(m.y, 25), windowHeight-25)
    paddle1.coord = Vec(paddle1.coord.x, y_coord)
    paddle2.coord = Vec(paddle2.coord.x, y_coord)
  })

  physics.addPhysicals(paddle1, paddle2, Ball)

  action {
    physics.step()
    if(right_edge.isTouching(Ball) || left_edge.isTouching(Ball)) restart()
  }

  backgroundColor = BLACK
  interface {
    print(count, windowWidth/2, windowHeight-20, WHITE)
  }
}

import com.github.dunnololda.scageprojects.pong.Pong._

object Ball extends DynaBall(windowCenter, 10, restitution = true) {
  var ball_speed = 50f

  init {
    ball_speed = 50
    coord = windowCenter
    velocity = Vec(math.random, math.random).n*ball_speed
  }

  action {
    if(velocity.norma < ball_speed) velocity = velocity.n*ball_speed
    if(math.abs(velocity.x) < 10) velocity = Vec(10*math.signum(velocity.x), velocity.y)
  }

  render {
    drawFilledCircle(coord, 10, RED)
  }
}

class Paddle(init_coord:Vec) extends StaticBox(init_coord, 10, 50, true) {
  action {
    if(isTouching(Ball)) {
      if(Ball.ball_speed < 100) Ball.ball_speed *= 1.05f
      count += 10
    }
  }

  render {
    drawFilledPolygon(points, WHITE)
  }
}
