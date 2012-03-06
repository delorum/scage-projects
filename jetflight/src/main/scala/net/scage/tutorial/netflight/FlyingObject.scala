package net.scage.tutorial.netflight

import net.scage.support.Vec

trait FlyingObject {
  protected var _speed = 5.0f
  def speed = _speed

  protected var _rotation = 0.0f
  def rotation = _rotation

  def step = Vec(-0.4f * _speed * math.sin(math.toRadians(_rotation)).toFloat,
    0.4f * _speed * math.cos(math.toRadians(_rotation)).toFloat)
}
