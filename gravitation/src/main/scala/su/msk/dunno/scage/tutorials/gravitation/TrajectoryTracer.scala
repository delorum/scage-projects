package su.msk.dunno.scage.tutorials.gravitation

import net.scage.support.tracer3.CoordTracer
import net.scage.support.Vec
import net.scage.ScageLib._
import Orbita._
import collection.mutable.ArrayBuffer
import org.lwjgl.opengl.GL11

object TrajectoryTracer extends CoordTracer[MaterialPoint] {
  val iterations_per_moment = property("trajectory.iterations", 5)

  private var points_count = 0
  val num_points = property("trajectory.points", 5000)

  private var _time = 0L

  private val points = ArrayBuffer[Vec]()

  private val ship_points = ArrayBuffer[(Vec, Long)]()
  private var ship_marker = Vec.zero
  private var marker_position = 0
  private var marker_rotation = 0f
  private var marker_direction = Vec.zero
  private var marker_acceleration_period = 0
  private var marker_acceleration = Vec.zero

  private var is_calculating_trajectories = false
  def calculateTrajectories() {
    is_calculating_trajectories = true
    points.clear()
    ship_points.clear()
    removeAllTraces()
    points_count = 0

    ship_marker = Orbita.ship.location
    marker_position = 0
    marker_rotation = Orbita.ship.rotation
    marker_direction = Orbita.ship.direction
    marker_acceleration_period = 0
    marker_acceleration = Vec.zero

    _time = OrbitalTracer.time
    center = ship_marker
    OrbitalTracer.tracesList.foreach(body => {
      if(body.id != Orbita.ship.id) addTrace(body.location, body.copy)
      else {
        val ship_copy = body.copy
        ship_copy.addFlightPlan(flight_plan)
        addTrace(body.location, ship_copy)
      }
    })
  }

  def stopCalculating() {
    is_calculating_trajectories = false
    Orbita.ship.addFlightPlan(flight_plan)
  }

  actionOnPause {
    if(is_calculating_trajectories && points_count < num_points) {
      for(i <- 1 to iterations_per_moment) {
        val new_body_coords = traces_list.foldLeft(List[(MaterialPoint, Vec)]())((bodies_coords, body) => {
          body.updateVelocity(_time, traces_list)
          val new_body_coord = outsideCoord(body.location + body.velocity*dt)
          (body, new_body_coord) :: bodies_coords
        })
        new_body_coords.foreach {
          case (body, coord) => {
            updateLocation(body, coord)
            if(body.id != Orbita.ship.id) points      += body.location.copy
            else                          ship_points += ((body.location.copy, _time))
          }
        }
        points_count += 1
        _time += 1
      }
    }
  }

  render {
    if(is_calculating_trajectories) {
      drawPoints(points, GREEN)
      drawPoints(ship_points.map(_._1), CYAN)
      ship_points.filter(_._2 % 500 == 0).foreach(sp => print(sp._2, sp._1, max_font_size/globalScale, CYAN))

      // draw ship marker
      currentColor = CYAN
      GL11.glPushMatrix()
      GL11.glTranslatef(ship_marker.x, ship_marker.y, 0.0f)
      GL11.glRotatef(marker_rotation, 0.0f, 0.0f, 1.0f)
      drawFilledPolygon(Vec(-1,-1), Vec(1,-1), Vec(0,2))
      GL11.glPopMatrix()

      drawLine(ship_marker, ship_marker+marker_direction*10, CYAN)

      flight_plan.foreach {
        case (pos, _, _, direction, _, _) =>
          drawFilledCircle(pos, 3f/globalScale, RED)
          drawLine(pos, pos+direction*10, RED)
      }
    }
  }

  // flight plan
  private val flight_plan = ArrayBuffer[(Vec, Long, Float, Vec, Vec, Int)]() // Position, Time, Rotation, Acceleration, AccelerationPeriod

  // time movement
  private var key_right_period = 50
  keyOnPause(KEY_UP, key_right_period,
    onKeyDown = {
      if(marker_position < ship_points.length-1) {
        marker_position += 1
        ship_marker = ship_points(marker_position)._1
        if(key_right_period > 1) key_right_period -= 1
      }
    },
    onKeyUp = {key_right_period = 50})
  private var key_left_period = 50
  keyOnPause(KEY_DOWN, 50,
    onKeyDown = {
      if(marker_position > 0) {
        marker_position -= 1
        ship_marker = ship_points(marker_position)._1
        if(key_left_period > 1) key_left_period -= 1
      }
    },
    onKeyUp = {key_left_period = 50})

  // rotation
  keyOnPause(KEY_LEFT, 50, onKeyDown = {
    marker_rotation += 5
    marker_direction = marker_direction.rotateDeg(5)
  })
  keyOnPause(KEY_RIGHT, 50, onKeyDown = {
    marker_rotation -= 5
    marker_direction = marker_direction.rotateDeg(-5)
  })

  // acceleration period
  private var key_numpad8_period = 50
  keyOnPause(KEY_NUMPAD8, 100,
    onKeyDown = {
      marker_acceleration_period += 1
      if(key_numpad8_period > 5) key_numpad8_period -= 1
    }, onKeyUp = {key_numpad8_period = 50})
  private var key_numpad2_period = 50
  keyOnPause(KEY_NUMPAD2, 100,
    onKeyDown = {
      marker_acceleration_period -= 1
      if(key_numpad2_period > 5) key_numpad2_period -= 1
    }, onKeyUp = {key_numpad2_period = 50})

  // acceleration
  private var key_numpad9_period = 50
  keyOnPause(KEY_NUMPAD9, key_numpad9_period,
    onKeyDown = {
      marker_acceleration += marker_direction.n*0.1f
      if(key_numpad9_period > 5) key_numpad9_period -= 1
    }, onKeyUp = {key_numpad9_period = 50})
  keyOnPause(KEY_NUMPAD6, onKeyDown = {marker_acceleration = Vec.zero})
  private var key_numpad3_period = 50
  keyOnPause(KEY_NUMPAD3, key_numpad3_period,
    onKeyDown = {
      marker_acceleration -= marker_direction.n*0.1f
      if(key_numpad3_period > 5) key_numpad3_period -= 1
    }, onKeyUp = {key_numpad3_period = 50})

  keyOnPause(KEY_SPACE, onKeyDown = {
    flight_plan += ((ship_marker, ship_points(marker_position)._2, marker_rotation, marker_direction, marker_acceleration, marker_acceleration_period))
    TrajectoryTracer.calculateTrajectories()
  })

  keyOnPause(KEY_C, onKeyDown = {
    flight_plan.clear()
  })

  keyOnPause(KEY_S, onKeyDown = {
    println("pause. S pressed")
    if(center == ship_marker) center = sun.location
    else center = ship_marker
  })

  interface {
    if(onPause) {
      print("acceleration: " + marker_acceleration,               10, windowHeight-20, CYAN)
      print("period: "       + marker_acceleration_period,        10, windowHeight-40, CYAN)
      if(ship_points.length > marker_position) {
        print("time: "       + ship_points(marker_position)._2,   10, windowHeight-60, CYAN)
      }
      print("flight plan: "  + flight_plan.length+" steps",       10, windowHeight-80, CYAN)
    }
  }
}