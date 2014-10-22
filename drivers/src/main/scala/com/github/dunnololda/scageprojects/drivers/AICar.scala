package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

// 5px = 1m
// ориентация: отсчитываем градусы от вектор Vec(0,1):
// против часов стрелки 0 -> +180
// по часовой стрелке   0 -> -180

// скорость подразумевается только положительная. Задним ходом пока ездить не умеем

class AICar(start_pos:Vec, start_rotation:Float, screen:Screen) {
  private val _path = ArrayBuffer[Vec]()
  def addWayPoint(p:Vec) {
    _path += p
  }
  def insertFirstWayPoint(p:Vec) {
    _path.insert(0, p)
  }
  def setWayPoint(p:Vec) {
    _path.clear()
    _path += p
  }
  def path:Seq[Vec] = _path

  private var _speed = 0f           // скорость в пикселях в  секунду
  def speed = _speed

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

  private var car_direction = Vec(0,1)  // единичный вектор, показывающий ориентацию машины

  private var need_wheel_rotation = 0f
  private var need_speed = 0f

  // AI машины, корректирующий траекторию. Вызывается каждые 50 мсек
  screen.action(100) {
    need_wheel_rotation = if(_path.nonEmpty) {
      val need_rotation = (_path.head - car_center).mydeg(def_vector)
      val tmp1 = need_rotation - _rotation
      val tmp = if(math.abs(tmp1) < 180) tmp1 else (360 - math.abs(tmp1))*math.signum(tmp1)*(-1)
      val dist = _path.head.dist(car_center) / 5f      // дистанция в метрах
      if(dist < 5 && math.abs(tmp) > 70) {
        // если точка близко к машине, но она не смотрит на нее прямо, чтобы избежать бесконечного кружения, добавляем к маршруту точку в отдалении,
        // от которой машина развернется к этой
        insertFirstWayPoint(car_center + car_direction.rotateDeg(45).n*20*5f)
        need_wheel_rotation
      } else {
        if(tmp < 0) math.max(tmp, -45) else math.min(tmp, 45)
      }
    } else 0
    need_speed = if(_path.nonEmpty) {
      val dist = _path.head.dist(car_center) / 5f      // дистанция в метрах

      // если дистанция меньше 10 метров и надо поворачивать на существенный угол - 1 км/ч
      if(dist < 10) {
        if(math.abs(need_wheel_rotation) > 30) 1f/3.6f*10f
        else 5f/3.6f*10f
      } else {
        if(math.abs(need_wheel_rotation) > 30) 10f/3.6f*10f
        else 30f/3.6f*10f
      }
    } else 0

    if(_path.nonEmpty) {
      val dist = _path.head.dist(car_center) / 5f // дистанция в метрах
      if(dist < 3) _path.remove(0)
    }
  }

  // повороты руля
  screen.action(5) {
    if(_front_wheels_rotation < need_wheel_rotation) {
      frontWheelsRotation += 1
      is_wheel_rotating = true
    } else if(_front_wheels_rotation > need_wheel_rotation) {
      frontWheelsRotation -= 1
      is_wheel_rotating = true
    } else is_wheel_rotating = false
  }

  // если требуется увеличить скорость
  screen.action(50) {
    if(_speed < need_speed) _speed += 0.5f
  }

  // если требуется торможение
  screen.action(25) {
    if(_speed > need_speed) {
      _speed -= 1
      if(_speed < 0) _speed = 0
    }
  }

  // физика движения машины, вызывается 60 раз в секунду
  screen.action {
    if(_speed != 0) {
      if(math.abs(_front_wheels_rotation) < 1f) _front_wheels_rotation = 0
      if(_front_wheels_rotation != 0) {
        // радиус поворота. 18.03 - типа расстояние между осями. Подобрал такой коэффициент, чтобы минимальный радиус разворота была 5.1 метра (при отклонении
        // колес на 45 градусов), как у шкоды октавии :)
        val r = 18.03f/math.sin(_front_wheels_rotation/180f*math.Pi)

        // радиус поворота r известен, линейная скорость speed известна, посчитаем угловую по формуле w = v/r
        val w = _speed/r/math.Pi*180f

        // speed у нас в пикселях в 1/60 секунды. r в пикселях. w получилась в градусах в 1/60 секунды
        rotation += (w/60f).toFloat

        // возвращаем руль на место, если водитель не удерживает его
        if(!is_wheel_rotating) {
          if(_speed > 0) _front_wheels_rotation -= (w/60f).toFloat
          else if(_speed < 0) _front_wheels_rotation += (w/60f).toFloat
        }
      }
      car_center += car_direction*_speed/60f
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
      drawFilledRectCentered(Vec(0,1.5f), 1,1, RED)
    }
    openglLocalTransform {
      openglMove(Vec(5,  26/2-4))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
      drawFilledRectCentered(Vec(0,1.5f), 1,1, RED)
    }

    // задние колеса
    drawFilledRectCentered(Vec(-5, -26/2+4), 1,4, WHITE)
    drawFilledRectCentered(Vec( 5, -26/2+4), 1,4, WHITE)
  }

  screen.interface {
    if(_path.nonEmpty) {
      val need_rotation = (_path.head - car_center).mydeg(def_vector)
      print(f"rotation is ${_rotation}%.1f deg", 20, 60, WHITE)
      print(f"need rotation is $need_rotation%.1f deg", 20, 40, WHITE)
      print(f"need wheel rotation is $need_wheel_rotation%.1f deg", 20, 20, WHITE)
    }
  }
}
