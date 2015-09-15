package com.github.dunnololda.scageprojects.scarcanoid

import com.github.dunnololda.scage.ScageLib._
import Scaranoid._

object PlayerBall extends DynaBall(Vec(windowWidth/2, windowHeight/2), property("ball.radius", 5)) {
  val ball_speed = property("ball.speed", 25)

  var ball_color = WHITE

  init {
    println ("me here!")
    coord = Vec(windowWidth/2, windowHeight/2)
    velocity = new Vec(-ball_speed, -ball_speed)
  }

  action {
    if(velocity.norma < ball_speed-1)
      velocity = velocity.n * ball_speed
    else if(math.abs(velocity.y) < 1)
      velocity = Vec(velocity.x, 10*math.signum(velocity.y))
  }

  render {
    currentColor = ball_color
    drawFilledCircle(coord, radius)
  }
}