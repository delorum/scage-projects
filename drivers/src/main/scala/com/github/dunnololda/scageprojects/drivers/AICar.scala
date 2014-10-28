package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

trait CarDefinition {
  def index:String

  def bodyState(seconds:Float):BodyState
  def bodyState(seconds:Float = 0, speed:Float, wheels_rotation:Float):BodyState

  def carCenter:Vec
  def rotation:Float
  def speed:Float
  def frontWheelsRotation:Float

  def needSpeed:Float
  def needRotation:Float
  def needWheelsRotation:Float

  def path:List[Vec]

  def distToNextPoint:Float
  def distToNextTurn:Float
}

// 5px = 1m
// ориентация: отсчитываем градусы от вектор Vec(0,1):
// против часов стрелки 0 -> +180
// по часовой стрелке   0 -> -180

// скорость подразумевается только положительная. Задним ходом пока ездить не умеем

class AICar(val index:String, start_pos:Vec, start_rotation:Float, screen:Screen) {
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
  def addPath(p:Seq[Vec]) {
    _path ++= p
  }
  def path:Seq[Vec] = _path

  private var _speed = 0f           // скорость в пикселях в  секунду
  def speed = _speed

  private var car_direction = Vec(0,1)  // единичный вектор, показывающий ориентацию машины

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

  private var need_wheels_rotation = 0f
  def needWheelsRotation = need_wheels_rotation

  private var need_speed = 0f
  def needSpeed = need_speed

  def needRotation =  _path.headOption.map(p => def_vector.signedDeg(p - car_center)).getOrElse(_rotation)

  def distToNextPoint:Float =  _path.headOption.map(p => p.dist(car_center) / 5f).getOrElse(0)
  def distToNextTurn:Float =  {
    if(_path.nonEmpty) {
      if(_path.length == 1) distToNextPoint else {
        val turn_point = _path.sliding(2).find {
          case Seq(from, to) => (_path.head - car_center).deg(to - from) > 20
        }.map(x => x.head).getOrElse(_path.head)
        turn_point.dist(car_center) / 5f
      }
    } else 0f
  }

  def bodyState(seconds_from_now:Float = 0, speed:Float = _speed, front_wheels_rotation:Float = _front_wheels_rotation):BodyState = BodyState(
    index,
    mass = 1800,
    acc = Vec.zero,
    vel = Vec.zero,
    coord = {
      if(speed == 0) car_center
      else {
        if(front_wheels_rotation == 0) {
          car_center + car_direction*speed*seconds_from_now
        } else {
          // радиус поворота. 13.4 - расстояние между осями колес, то бишь "колесная база"
          val r = 13.4f/math.sin(front_wheels_rotation.toRad)

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
      if(front_wheels_rotation == 0) _rotation
      else {
        // радиус поворота. 13.4 - расстояние между осями колес
        val r = 13.4f/math.sin(front_wheels_rotation.toRad)

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
  
  def collisionIfSuchSpeedAndRotation(seconds:List[Float], speed:Float, front_wheels_rotation:Float):Boolean = {
    seconds.exists(second => {
      val our_state = bodyState(second, speed, front_wheels_rotation)
      val other_cars_states_after = AICarTestArea.future_other_cars_states(second)(index)
      other_cars_states_after.exists(other_car_state => maybeCollision(our_state, other_car_state).nonEmpty)
    })
  }

  def distanceBecomeIfSuchSpeedAndRotation(distance:Float, speed:Float, front_wheels_rotation:Float):Boolean = {
    AICarTestArea.seconds.exists(second => {
      val our_state = bodyState(second, speed, front_wheels_rotation)
      val other_cars_states_now_and_after = AICarTestArea.other_cars_states_now_and_after(second)(index)
      other_cars_states_now_and_after.exists{case (other_car_state_now, other_car_state_after) =>
        val d1 = car_center.dist(other_car_state_now.coord)
        val d2 = our_state.coord.dist(other_car_state_after.coord)
        d1 > distance && d2 < distance
      }
    })
  }

  // AI машины, корректирующий траекторию. Вызывается каждые 100 мсек
  screen.action(100) {
    val moment = AICarTestArea.msecsFromAppStart
    if(_path.nonEmpty) {
      val need_rotation = def_vector.signedDeg(_path.head - car_center)
      val tmp1 = need_rotation - _rotation
      val tmp = if(math.abs(tmp1) < 180) tmp1 else (360 - math.abs(tmp1))*math.signum(tmp1)*(-1)
      val dist_to_next_point = _path.head.dist(car_center) / 5f      // дистанция в метрах до следующей точки
      val dist_to_next_turn = if(_path.length == 1) dist_to_next_point else {
        val turn_point = _path.sliding(2).find {
          case Seq(from, to) => (_path.head - car_center).deg(to - from) > 5
        }.map(x => x.head).getOrElse(_path.head)
        turn_point.dist(car_center) / 5f
      }
      if(dist_to_next_point < 5 && math.abs(tmp) > 80) {
        println(s"$dist_to_next_point : $tmp")
        // если точка близко к машине, но она не смотрит на нее прямо, чтобы избежать бесконечного кружения, добавляем к маршруту точку в отдалении,
        // от которой машина развернется к этой
        insertFirstWayPoint(car_center + car_direction.rotateDeg(45).n*20*5f)
      } else {
        need_wheels_rotation = if(tmp < 0) math.max(tmp, -45) else math.min(tmp, 45)
      }
      need_speed = if (math.abs(need_wheels_rotation) > 30) {
        if (dist_to_next_point < 6) {
          if (math.abs(_front_wheels_rotation) >= 44) 10f / 3.6f * 5f
          else 5f / 3.6f * 5f
        } else 5f / 3.6f * 5f
      } else if (math.abs(need_wheels_rotation) > 10) 10f / 3.6f * 5f
      else {
        if (dist_to_next_turn > 20) 40f / 3.6f * 5f
        else 20f / 3.6f * 5f
      }
    } else {
      need_wheels_rotation = 0
      need_speed = 0
    }

    // алгоритм избежания столкновений
    val is_in_collision_now = {val bs = bodyState(); AICarTestArea.other_cars_states_now_and_after(AICarTestArea.seconds.head)(index).map(_._1).exists(other_car_bs => maybeCollision(bs, other_car_bs).nonEmpty)}
    if(is_in_collision_now) {
      // если уже столкнулись - пытаемся разъехаться
      if(!collisionIfSuchSpeedAndRotation(List(AICarTestArea.seconds.last), 5f / 3.6f * 5f, need_wheels_rotation)) {
        need_speed = 5f / 3.6f * 5f
        if(index == AICarTestArea.ai_car.index) {
          println(s"[$index] $moment. коллизия есть сейчас, если двигаться вперед со скоростью 5км/ч, коллизии не будет")
        }
      } else {
        if(!collisionIfSuchSpeedAndRotation(List(AICarTestArea.seconds.last), -5f / 3.6f * 5f, need_wheels_rotation)) {
          if(index == AICarTestArea.ai_car.index) {
            println(s"[$index] $moment. коллизия есть сейчас, если двигаться назад со скоростью 5км/ч, коллизии не будет")
          }
          need_speed = -5f / 3.6f * 5f
        } else {
          // куда ни двинь - коллизия сохраняется. Попрбуем двигаться туда, где увеличивается separation
          val our_state1 = bodyState(AICarTestArea.seconds.last, 5f / 3.6f * 5f, need_wheels_rotation)
          val other_cars_states_after1 = AICarTestArea.future_other_cars_states(AICarTestArea.seconds.last)(index)
          val collision1 = other_cars_states_after1.map(other_car_state => maybeCollision(our_state1, other_car_state)).flatten
          val our_state2 = bodyState(AICarTestArea.seconds.last, -5f / 3.6f * 5f, need_wheels_rotation)
          val other_cars_states_after2 = AICarTestArea.future_other_cars_states(AICarTestArea.seconds.last)(index)
          val collision2 = other_cars_states_after2.map(other_car_state => maybeCollision(our_state2, other_car_state)).flatten
          (collision1.headOption, collision2.headOption) match {
            case (Some(col1), Some(col2)) =>
              if(math.abs(col1.separation) < math.abs(col2.separation)) {
                if(index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. коллизия есть сейчас и будет куда ни двигайся, но если двигаться вперед, separation уменьшается")
                }
                need_speed = 5f / 3.6f * 5f
              } else {
                if(index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. коллизия есть сейчас и будет куда ни двигайся, но если двигаться назад, separation уменьшается")
                }
                need_speed = -5f / 3.6f * 5f
              }
            case (None, Some(col2)) =>
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия есть сейчас и будет куда ни двигайся, но если двигаться вперед, ее вроде как не будет...")
              }
              need_speed = 5f / 3.6f * 5f
            case (Some(col1), None) =>
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия есть сейчас и будет куда ни двигайся, но если двигаться назад, ее вроде как не будет...")
              }
              need_speed = -5f / 3.6f * 5f
            case _ =>
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия есть сейчас и будет куда ни двигайся, какая-то вообще левая опция, по которой двигаемся вперед...")
              }
              need_speed = 5f / 3.6f * 5f
          }
        }
      }
    } else {
      // если при сохранении текущих параметров скорости/направления мы столкнемся через столько-то секунд - 
      // сдаем назад или вперед или стоим ждем. При этом, мы стараемся сохранить запланированное положение руля, потому что
      // нам также важно не отдалится от точки пути, к которой мы едем
      if(collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, _speed, _front_wheels_rotation)) {
        if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, 5f / 3.6f * 5f, need_wheels_rotation)) {
          if(index == AICarTestArea.ai_car.index) {
            println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до 5км/ч и сохранить направление")
          }
          need_speed = 5f / 3.6f * 5f
        } else {
          if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, need_wheels_rotation)) {
            if(index == AICarTestArea.ai_car.index) {
              println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч и сохранить направление")
            }
            need_speed = -5f / 3.6f * 5f
          } else {
            if(index == AICarTestArea.ai_car.index) {
              println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, и будет, если сбрасывать до 5км/ч или до -5 км/ч, так что останавливаемся")
            }
            need_speed = 0
          }
        }
      } else {
        // раз мы здесь, значит текущие скорость и направление в норме. Теперь надо проверить запланированные.
        if (collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, need_speed, need_wheels_rotation)) {
          // Запланированные скорость и направление ведут к коллизии. Казалось бы, мы можем просто приравнять их текущим, ведь те в норме
          // Но все не так просто: текущие скорость/направление могут отдалить нас от точки пути, к которой мы едем. Так что мы постараемся
          // сохранить направление
          if(_speed > 5f / 3.6f * 5f) {
            if (!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, _speed, need_wheels_rotation)) {
              if (index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, но ее не будет, если двигаться с текущей скоростью, но сохранить запланированное направление")
              }
              need_speed = _speed
            } else {
              if (!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, 5f / 3.6f * 5f, need_wheels_rotation)) {
                if (index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, но ее не будет, если сбросить до 5км/ч, но сохранить запланированное направление")
                }
                need_speed = 5f / 3.6f * 5f
              } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, need_wheels_rotation)) {
                if(index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч, но сохранить запланированное направление")
                }
                need_speed = -5f / 3.6f * 5f
              } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, 0)) {
                if(index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч и не крутить рулем")
                }
                need_speed = -5f / 3.6f * 5f
                need_wheels_rotation = 0
              } else {
                if (index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, и будет, если сбросить до 5км/ч или до -5км/ч и не крутить рулем, так что останавливаемся")
                }
                need_speed = 0
              }
            }
          } else if(_speed != 0) {
            // в эту секцию попадаем, если _speed меньше 5км/ч. Но _speed меняется, только чтобы достичь need_speed,
            // а need_speed не может быть ноль и меньше -5км/ч. То есть -5км/ч < _speed < 5км/ч
            if (!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, 5f / 3.6f * 5f, need_wheels_rotation)) {
              if (index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, но ее не будет, если сбросить до 5км/ч, но сохранить запланированное направление")
              }
              need_speed = 5f / 3.6f * 5f
            } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, _speed, need_wheels_rotation)) {
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если двигаться с текущей скоростью, но сохранить запланированное направление")
              }
              need_speed = _speed
            } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, need_wheels_rotation)) {
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч, но сохранить запланированное направление")
              }
              need_speed = -5f / 3.6f * 5f
            } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, 0)) {
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч и не крутить рулем")
              }
              need_speed = -5f / 3.6f * 5f
              need_wheels_rotation = 0
            } else {
              if (index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, и будет, если сбросить до 5км/ч или до -5км/ч и не крутить рулем, так что останавливаемся")
              }
              need_speed = 0
            }
          } else {
            if (!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, 5f / 3.6f * 5f, need_wheels_rotation)) {
              if (index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, но ее не будет, если сбросить до 5км/ч, но сохранить запланированное направление")
              }
              need_speed = 5f / 3.6f * 5f
            } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, need_wheels_rotation)) {
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч, но сохранить запланированное направление")
              }
              need_speed = -5f / 3.6f * 5f
            } else if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, 0)) {
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с текущими скоростью/направлением, но ее не будет, если сбросить до -5км/ч и не крутить рулем")
              }
              need_speed = -5f / 3.6f * 5f
              need_wheels_rotation = 0
            } else {
              if (index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. коллизия случится, если двигаться с запланированными скоростью/направлением, и будет, если сбросить до 5км/ч или до -5км/ч и не крутить рулем, так что останавливаемся")
              }
              need_speed = 0
            }
          }
        } else {
          // попытаемся еще соблюдать дистанцию 3 метра
          if(distanceBecomeIfSuchSpeedAndRotation(3f*5f, _speed, _front_wheels_rotation)) {
            if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, 5f / 3.6f * 5f, need_wheels_rotation)) {
              if(index == AICarTestArea.ai_car.index) {
                println(s"[$index] $moment. дистанция сократится, если двигаться с текущими скоростью/направлением, но коллизии не будет, если сбросить до 5км/ч и сохранить направление")
              }
              need_speed = 5f / 3.6f * 5f
            } else {
              if(!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, need_wheels_rotation)) {
                if(index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. дистанция сократится, если двигаться с текущими скоростью/направлением, но коллизии не будет, если сбросить до -5км/ч и сохранить направление")
                }
                need_speed = -5f / 3.6f * 5f
              } else {
                if(index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. дистанция сократится, если двигаться с текущими скоростью/направлением, и будет коллизия, если сбросить до 5км/ч или до -5км/ч и сохранить направление, так что останавливаемся")
                }
                need_speed = 0
              }
            }
          } else {
            if (distanceBecomeIfSuchSpeedAndRotation(3f * 5f, need_speed, need_wheels_rotation)) {
              if (!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, 5f / 3.6f * 5f, 0)) {
                if (index == AICarTestArea.ai_car.index) {
                  println(s"[$index] $moment. дистанция сократится, если двигаться с запланированными скоростью/направлением, но коллизии не будет, если сбросить до 5км/ч и не крутить рулем")
                }
                need_speed = 5f / 3.6f * 5f
                need_wheels_rotation = 0
              } else {
                if (!collisionIfSuchSpeedAndRotation(AICarTestArea.seconds, -5f / 3.6f * 5f, 0)) {
                  if (index == AICarTestArea.ai_car.index) {
                    println(s"[$index] $moment. дистанция сократится, если двигаться с запланированными скоростью/направлением, но коллизии не будет, если сбросить до -5км/ч и не крутить рулем")
                  }
                  need_speed = -5f / 3.6f * 5f
                  need_wheels_rotation = 0
                } else {
                  if (index == AICarTestArea.ai_car.index) {
                    println(s"[$index] $moment. дистанция сократится, если двигаться с запланированными скоростью/направлением, и будет коллизия, если сбросить до 5км/ч или до -5км/ч и не крутить рулем, так что останавливаемся")
                  }
                  need_speed = 0
                }
              }
            }
          }
        }
      }
    }

    if(_path.nonEmpty) {
      val dist = _path.head.dist(car_center) / 5f // дистанция в метрах
      if(dist < 3) _path.remove(0)
    }
  }

  // повороты руля
  screen.action(5) {
    if(_front_wheels_rotation < need_wheels_rotation &&
      need_wheels_rotation - _front_wheels_rotation > 1) {
      frontWheelsRotation += 1
      is_wheel_rotating = true
    } else if(_front_wheels_rotation > need_wheels_rotation &&
              _front_wheels_rotation - need_wheels_rotation > 1) {
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
      //if(_speed < 0) _speed = 0
    }
  }

  // физика движения машины, вызывается 60 раз в секунду. Поэтому величины, которые "<что-то> в секунду", делим на 60
  screen.action {
    if(_speed != 0) {
      if(math.abs(_front_wheels_rotation) < 1f) _front_wheels_rotation = 0
      if(_front_wheels_rotation != 0) {
        // радиус поворота. 13.4 - расстояние между осями колес
        val r = 13.4f/math.sin(_front_wheels_rotation.toRad)

        // радиус поворота r известен, линейная скорость speed известна, посчитаем угловую по формуле w = v/r
        val w = (_speed/r).toDeg

        // speed у нас в пикселях в секунду. r в пикселях. w получилась в градусах в  секунду
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
    drawRectCentered(Vec.zero, 9, 23.3f, WHITE)

    // передние колеса
    openglLocalTransform {
      openglMove(Vec(-4.5f,  6.7f))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
      drawFilledRectCentered(Vec(0,1.5f), 1,1, RED)
    }
    openglLocalTransform {
      openglMove(Vec(4.5f,  6.7f))
      openglRotateDeg(_front_wheels_rotation)
      drawFilledRectCentered(Vec.zero, 1,4, WHITE)
      drawFilledRectCentered(Vec(0,1.5f), 1,1, RED)
    }

    // задние колеса
    drawFilledRectCentered(Vec(-4.5f, -6.7f), 1,4, WHITE)
    drawFilledRectCentered(Vec( 4.5f, -6.7f), 1,4, WHITE)
  }
}
