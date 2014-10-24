package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

class Car(val index:String, start_pos:Vec, start_rotation:Float, screen:Screen) {
  var speed = 0f

  private var _rotation = 0f        // угол в градусах между вектором Vec(0,1) и текущим направлением машины
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

  def bodyState(seconds_from_now:Int):BodyState = BodyState(
    index,
    mass = 1800,
    acc = Vec.zero,
    vel = Vec.zero,
    coord = {
      if(speed == 0) car_center
      else {
        if(_front_wheels_rotation == 0) {
          car_center + car_direction*speed*seconds_from_now
        } else {
          // радиус поворота. 13.4 - расстояние между осями колес
          val r = 13.4f/math.sin(_front_wheels_rotation.toRad)

          // радиус поворота r известен, линейная скорость speed известна, посчитаем угловую по формуле w = v/r
          val w = (speed/r).toDeg

          // speed у нас в пикселях в секунду. r в пикселях. w получилась в градусах в секунду
          // за время seconds_from_now будет сделан угол w*seconds_from_now. Нас интересует половина этого угла
          val a = w*seconds_from_now

          // a в градусах! При подстановке его в следующие формулы под синус не забываем переводить в радианы!

          // машина за это время переместится на такое расстояние, описывая дугу окружности радиуса r:
          val len = 2f*r*math.sin(a.toRad/2f)

          car_center + car_direction.rotateDeg(a/2f).n*len
        }
      }
    },
    ang_acc = 0.0,
    ang_vel = 0.0,
    ang = {
      if(_front_wheels_rotation == 0) _rotation
      else {
        // радиус поворота. 13.4 - расстояние между осями колес
        val r = 13.4f/math.sin(_front_wheels_rotation.toRad)

        // радиус поворота r известен, линейная скорость speed известна, посчитаем угловую по формуле w = v/r
        val w = (speed/r).toDeg

        // speed у нас в пикселях в секунду. r в пикселях. w получилась в градусах в секунду
        // за время seconds_from_now будет сделан угол w*seconds_from_now.

        (_rotation + w*seconds_from_now) % 360
      }
    },
    shape = BoxShape(9, 23.3),
    is_static = false
  )

  def bodyState2(seconds_from_now:Int):BodyState = BodyState(
    index,
    mass = 1800,
    acc = Vec.zero,
    vel = Vec.zero,
    coord = {
      if(speed == 0) car_center
      else {
        if(_front_wheels_rotation == 0) {
          car_center + car_direction*speed*seconds_from_now
        } else {
          val radius = 13.4f/math.sin(_front_wheels_rotation.toRad)
          val w = (speed/radius).toDeg
          val a = w*seconds_from_now
          val len = 2f*radius*math.sin(a/2f)
          val (rcc, rcd, rr) = (0 to seconds_from_now*60).foldLeft((car_center, car_direction, _rotation)) {
            case ((cc, cd, r), tact) =>
              val nr = {
                var tmp = (r + w.toFloat/60f) % 360
                if(tmp > 180)       tmp = tmp - 360
                else if(tmp < -180) tmp = tmp + 360
                tmp
              }
              val ncd = def_vector.rotateDeg(nr).n
              val ncc = cc + ncd*speed/60f
              (ncc, ncd, nr)
          }
          val aa = rr - _rotation
          val lenlen = rcc.dist(car_center)
          rcc
        }
      }
    },
    ang_acc = 0.0,
    ang_vel = 0.0,
    ang = {
      if(_front_wheels_rotation == 0) _rotation
      else {
        val radius = 13.4f/math.sin(_front_wheels_rotation.toRad)
        val w = (speed/radius).toDeg
        val (rcc, rcd, rr) = (0 to seconds_from_now*60).foldLeft((car_center, car_direction, _rotation)) {
          case ((cc, cd, r), sec) =>
            val nr = {
              var tmp = (r + w.toFloat/60f) % 360
              if(tmp > 180)       tmp = tmp - 360
              else if(tmp < -180) tmp = tmp + 360
              tmp
            }
            val ncd = def_vector.rotateDeg(nr).n
            val ncc = cc + ncd*speed/60f
            (ncc, ncd, nr)
        }
        rr
      }
    },
    shape = BoxShape(9, 23.3),
    is_static = false
  )

  screen.action {
    if(speed != 0) {
      if(math.abs(_front_wheels_rotation) < 1f) _front_wheels_rotation = 0
      if(_front_wheels_rotation != 0) {
        // радиус поворота. 13.4 - расстояние между осями колес
        val r = 13.4f/math.sin(_front_wheels_rotation.toRad)

        // радиус поворота r известен, линейная скорость speed известна, посчитаем угловую по формуле w = v/r
        val w = (speed/r).toDeg

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
    drawRectCentered(Vec.zero, 9, 23.3f, WHITE)

    // передние колеса
    openglLocalTransform {
      openglMove(Vec(-4.5f,  6.7f))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
    }
    openglLocalTransform {
      openglMove(Vec(4.5f,  6.7f))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
    }

    // задние колеса
    drawFilledRectCentered(Vec(-4.5f, -6.7f), 1,4, WHITE)
    drawFilledRectCentered(Vec( 4.5f, -6.7f), 1,4, WHITE)
  }
}
