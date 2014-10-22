package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

class Car(start_pos:Vec, start_rotation:Float, screen:Screen) {
  var speed = 0f

  private var _rotation = 0f        // угол в градусах между вектором Vec(0,1) и текущим направлением машины
  private lazy val def_vector = Vec(0,1)  // просто чтобы не пересоздавать постоянно
  def rotation_=(new_rotation:Float) {
    _rotation = new_rotation % 360
    if(_rotation > 180)       _rotation = _rotation - 360
    else if(_rotation < -180) _rotation = _rotation + 360
    car_direction = def_vector.rotateDeg(_rotation).n
  }
  def rotation:Float = _rotation
  rotation = start_rotation

  private var _front_wheels_rotation = 0f        // угол в градусах между вектором Vec(0,1) и текущим направлением машины
  def frontWheelsRotation_=(new_rotation:Float) {
    _front_wheels_rotation = new_rotation  }
  def frontWheelsRotation:Float = _front_wheels_rotation

  var is_wheel_rotating = false

  private var car_center:Vec = start_pos
  def carCenter = car_center

  private var car_direction = Vec(0,1)

  screen.action {
    if(speed != 0) {
      if(math.abs(_front_wheels_rotation) < 1f) _front_wheels_rotation = 0
      if(_front_wheels_rotation != 0) {
        // радиус поворота. 18.03 - типа расстояние между осями. Подобрал такой коэффициент, чтобы минимальный радиус разворота была 5.1 метра (при отклонении
        // колес на 45 градусов), как у шкоды октавии :)
        val r = 18.03f/math.sin(_front_wheels_rotation/180f*math.Pi)

        // радиус поворота r известен, линейная скорость speed известна, посчитаем угловую по формуле w = v/r
        val w = speed/r/math.Pi*180f

        // speed у нас в пикселях в 1/60 секунды. r в пикселях. w получилась в градусах в 1/60 секунды
        rotation += (w/60f).toFloat

        // возвращаем руль на место, если водитель не удерживает его
        if(!is_wheel_rotating) {
          if(speed > 0) _front_wheels_rotation -= (w/60f).toFloat
          else if(speed < 0) _front_wheels_rotation += (w/60f).toFloat
        }
      }
      car_center += car_direction*speed/60f
    }
  }

  screen.render {
    openglMove(car_center)
    openglRotateDeg(_rotation)
    drawRectCentered(Vec.zero, 10, 26, WHITE)

    // передние колеса
    openglLocalTransform {
      openglMove(Vec(-5,  26/2-4))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
    }
    openglLocalTransform {
      openglMove(Vec(5,  26/2-4))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
    }

    // задние колеса
    drawFilledRectCentered(Vec(-5, -26/2+4), 1,4, WHITE)
    drawFilledRectCentered(Vec( 5, -26/2+4), 1,4, WHITE)
  }
}
