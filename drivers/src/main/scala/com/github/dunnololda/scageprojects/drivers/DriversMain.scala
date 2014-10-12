package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

// 5px = 1m

object DriversMain extends ScageScreenApp("Drivers", 1280, 1024) {
  val car = new Car(Vec(400+7, 300), this)

  key(KEY_UP,     50, onKeyDown = car.speed += 0.5f)
  key(KEY_DOWN,   50, onKeyDown = car.speed -= 0.5f)
  key(KEY_SPACE,  25, onKeyDown = {
    if(car.speed != 0) {
      if(car.speed < 0) {
        car.speed += 1
        if(car.speed > 0) car.speed = 0
      }
      if(car.speed > 0) {
        car.speed -= 1
        if(car.speed < 0) car.speed = 0
      }
    }

  })
  key(KEY_RIGHT,   5, onKeyDown = {car.is_wheel_rotating = true; if(car.frontWheelsRotation > -45) car.frontWheelsRotation -= 1}, onKeyUp = car.is_wheel_rotating = false)
  key(KEY_LEFT,    5, onKeyDown = {car.is_wheel_rotating = true; if(car.frontWheelsRotation <  45) car.frontWheelsRotation += 1}, onKeyUp = car.is_wheel_rotating = false)

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  center = car.carCenter+Vec(0,100)
  globalScale = 3
  rotationPoint = car.carCenter
  rotationAngleDeg = -car.rotation

  mouseWheelUp(onWheelUp = m => if(globalScale < 4) globalScale += 1)
  mouseWheelDown(onWheelDown = m => if(globalScale > 1) globalScale -= 1)

  /*render {
    // границы дороги
    drawLine(Vec(0, 300+30), Vec(800, 300+30), WHITE)
    drawLine(Vec(0, 300-30), Vec(800, 300-30), WHITE)

    // двойная сплошная посередине
    drawLine(Vec(0, 300+1), Vec(800, 300+1), WHITE)
    drawLine(Vec(0, 300-1), Vec(800, 300-1), WHITE)


    // пунктирные линии, разделяющие полосы
    drawDashedLine(Vec(0, 300+15), Vec(800, 300+15), 5, WHITE)
    drawDashedLine(Vec(0, 300-15), Vec(800, 300-15), 5, WHITE)
  }*/

  render {
    // границы дороги
    drawLine(Vec(0, 150+30), Vec(370, 150+30), WHITE)
    drawLine(Vec(0, 150-30), Vec(430, 150-30), WHITE)

    // двойная сплошная посередине
    drawLine(Vec(0, 150+1), Vec(400-1, 150+1), WHITE)
    drawLine(Vec(0, 150-1), Vec(400+1, 150-1), WHITE)

    // пунктирные линии, разделяющие полосы
    drawDashedLine(Vec(0, 150+15), Vec(385, 150+15), 5, WHITE)
    drawDashedLine(Vec(0, 150-15), Vec(415, 150-15), 5, WHITE)


    // границы дороги
    drawLine(Vec(370, 150+30), Vec(370, 600), WHITE)
    drawLine(Vec(430, 150-30), Vec(430, 300-30), WHITE)
    drawLine(Vec(430, 300+30), Vec(430, 600), WHITE)

    // двойная сплошная посередине
    drawLine(Vec(400-1, 150+1), Vec(400-1, 300-30), WHITE)
    drawLine(Vec(400+1, 150-1), Vec(400+1, 300-30), WHITE)
    drawLine(Vec(400-1, 300+30), Vec(400-1, 600), WHITE)
    drawLine(Vec(400+1, 300+30), Vec(400+1, 600), WHITE)

    // перекресток
    drawDashedLine(Vec(400, 300-30), Vec(400, 300+30), 5, WHITE)

    // пунктирные линии, разделяющие полосы
    drawDashedLine(Vec(385, 150+15), Vec(385, 600), 5, WHITE)
    drawDashedLine(Vec(415, 150-15), Vec(415, 600), 5, WHITE)


    // границы дороги
    drawLine(Vec(430, 300+30), Vec(800, 300+30), WHITE)
    drawLine(Vec(430, 300-30), Vec(800, 300-30), WHITE)

    // двойная сплошная посередине
    drawLine(Vec(430, 300+1), Vec(800, 300+1), WHITE)
    drawLine(Vec(430, 300-1), Vec(800, 300-1), WHITE)

    // пунктирные линии, разделяющие полосы
    drawDashedLine(Vec(430, 300+15), Vec(800, 300+15), 5, WHITE)
    drawDashedLine(Vec(430, 300-15), Vec(800, 300-15), 5, WHITE)


    // границы дороги
    drawLine(Vec(0-60, -600-60), Vec(0-60, 600+60), WHITE)
    drawLine(Vec(0, 150+30), Vec(0, 600), WHITE)
    drawLine(Vec(0, 150-30), Vec(0, -600), WHITE)

    // двойная сплошная посередине
    drawLine(Vec(0-30-1, -600-30-1), Vec(0-30-1, 600+30+1), WHITE)
    drawLine(Vec(0-30+1, -600-30+1), Vec(0-30+1, 600+30-1), WHITE)

    // пунктирные линии, разделяющие полосы

    // границы дороги
    drawLine(Vec(0-60, 600+60), Vec(800+60, 600+60), WHITE)
    drawLine(Vec(0, 600), Vec(370, 600), WHITE)
    drawLine(Vec(430, 600), Vec(800, 600), WHITE)

    // двойная сплошная посередине
    drawLine(Vec(0-30-1, 600+30+1), Vec(800+30+1, 600+30+1), WHITE)
    drawLine(Vec(0-30+1, 600+30-1), Vec(800+30-1, 600+30-1), WHITE)

    // пунктирные линии, разделяющие полосы

    // границы дороги
    drawLine(Vec(800+60, 600+60), Vec(800+60, -600-60), WHITE)
    drawLine(Vec(800, 600), Vec(800, 300+30), WHITE)
    drawLine(Vec(800, 300-30), Vec(800, -600), WHITE)

    // двойная сплошная посередине

    // пунктирные линии, разделяющие полосы

    // границы дороги
    drawLine(Vec(0, -600), Vec(800, -600), WHITE)
    drawLine(Vec(0-60, -600-60), Vec(800+60, -600-60), WHITE)

    // двойная сплошная посередине

    // пунктирные линии, разделяющие полосы
  }

  interface {
    //print(f"rotation radius: ${18.03f/math.sin(car.frontWheelsRotation/180f*math.Pi)/5f}%.1f m", 20, 80, WHITE)
    //print(f"wheels rotation: ${car.frontWheelsRotation}%.0f deg", 20, 60, WHITE)
    print(f"rotation: ${car.rotation}%.0f deg", 20, 40, WHITE)
    print(f"speed: ${car.speed/5}%.0f m/s; ${car.speed/5*3.6}%.0f km/h", 20, 20, WHITE)
  }

  def drawDashedLine(from:Vec, to:Vec, dash_len:Float, color:ScageColor): Unit = {
    val line_len = (to - from).norma
    val normal = (to - from).n
    (0f to line_len-dash_len by dash_len*2).foreach {
      case dash_from => drawLine(from + normal*dash_from, from+normal*(dash_from + dash_len), color)
    }
  }
}

class Car(start_pos:Vec, screen:Screen) {
  var speed = 0f

  private var _rotation = 0f        // угол в градусах между вектором Vec(0,1) и текущим направлением машины
  private lazy val def_vector = Vec(0,1)  // просто чтобы не пересоздавать постоянно
  def rotation_=(new_rotation:Float) {
    _rotation = new_rotation % 360
    car_direction = def_vector.rotateDeg(_rotation).n
  }
  def rotation:Float = _rotation

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
          if(speed > 0) _front_wheels_rotation -= (w/60f*2).toFloat
          else if(speed < 0) _front_wheels_rotation += (w/60f*2).toFloat
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