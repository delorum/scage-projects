package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageId
import com.github.dunnololda.scageprojects.orbitalkiller.ships._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.PhysicsUtils._

/**
  * Created by andrey on 1/7/18.
  */
trait ShipComponents extends ShipsAware with PlanetsAware { this: OrbitalComponents =>
  // стоим на поверхности Земли
  val ship_start_position = earth.coord + DVec(495, earth.radius + 3.5)
  val ship_init_velocity = earth.linearVelocity + (ship_start_position - earth.coord).p * earth.groundSpeedMsec /*DVec.zero*/

  // суборбитальная траектория
  //val ship_start_position = earth.coord + DVec(500, earth.radius + 100000)
  //val ship_init_velocity = speedToHaveOrbitWithParams(ship_start_position, -30000, earth.coord, earth.linearVelocity, earth.mass, G)

  // на круговой орбите в 200 км от поверхности Земли
  //val ship_start_position = earth.coord + DVec(0, 1).rotateDeg(170)*(earth.radius + 200000)
  //val ship_init_velocity = speedToHaveOrbitWithParams(ship_start_position, 0, earth.coord, earth.linearVelocity, earth.mass, G, ccw = true)

  //val ship_start_position = earth.coord + DVec(-100, earth.radius + 198000)
  //val ship_init_velocity = speedToHaveOrbitWithParams(ship_start_position, 900000, earth.coord, earth.linearVelocity, earth.mass, G, ccw = false)

  //val ship_start_position = earth.coord + DVec(-100, earth.radius + 199015)
  //val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)/** 1.15 */

  // стоим на поверхности Луны
  //val ship_start_position = moon.coord + DVec(500, moon.radius + 3.5)
  //val ship_init_velocity = moon.linearVelocity + (ship_start_position - moon.coord).p*moon.groundSpeedMsec/*DVec.zero*//*satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.15*/
  //val ship_init_velocity = -escapeVelocity(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.01

  // на орбите в 100 км от поверхности Луны
  //val ship_start_position = moon.coord + DVec(0, 1).rotateDeg(90)*(moon.radius + 100000)
  //val ship_init_velocity = speedToHaveOrbitWithParams(ship_start_position, 0, moon.coord, moon.linearVelocity, moon.mass, G, ccw = true)//satelliteSpeed(ship_start_position, moon.coord, moon.linearVelocity, moon.mass, G, counterclockwise = false)
  //val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.15

  // на гиперболической орбите Земли, приближаемся к перицентру, летим по часовой стрелке
  //val ship_start_position = DVec(-6.2797933836710215E7, -1.2455588349688923E8) + earth.coord
  //val ship_init_velocity = DVec(30521.418357148767,2855.1265848825283)

  // на гиперболической орбите Земли, приближаемся к перицентру, летим против часовой стрелки
  //val ship_start_position = DVec(9.594617648145294E7, -8.919468846308415E7) + earth.coord
  //val ship_init_velocity = DVec(28167.17922375556,2692.468259455251)

  val player_ship: Ship4 = new Ship4(ScageId.nextId,
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 0
  )

  // на круговой орбите в 200 км от поверхности Земли
  val station_start_position = earth.coord + DVec(-110, earth.radius + 199160)
  val station_init_velocity = satelliteSpeed(station_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)

  // суборбитальная траектория
  //val station_start_position = earth.coord + DVec(0, earth.radius + 100000)
  //val station_init_velocity = speedToHaveOrbitWithParams(station_start_position, -30000, earth.coord, earth.linearVelocity, earth.mass, G)

  val station = new SpaceStation2(ScageId.nextId,
    init_coord = station_start_position,
    init_velocity = station_init_velocity,
    init_rotation = 90
  )

  // случайная орбита с перигеем от 200 до 1000 км, и апогеем от 0 до 3000 км выше перигея
  val sat1_start_position = earth.coord + DVec(0, 1).rotateDeg(math.random * 360) * (earth.radius + 200000 + math.random * 800000)
  val sat1_init_velocity = speedToHaveOrbitWithParams(sat1_start_position, math.random * 3000000, earth.coord, earth.linearVelocity, earth.mass, G)
  //val sat1_start_position=DVec(1365327.0285981554, 6507689.41090233)
  //val sat1_init_velocity=DVec(21868.653743674382, 1661.8351848003101)
  println(s"sat1_start_position=$sat1_start_position")
  println(s"sat1_init_velocity=$sat1_init_velocity")


  // на круговой орбите в 200 км от поверхности Земли
  //val sat1_start_position = earth.coord + DVec(-200, earth.radius + 199000)
  //val sat1_init_velocity = satelliteSpeed(sat1_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)/** 1.15 */
  val sat1 = new Satellite1(ScageId.nextId,
    init_coord = sat1_start_position,
    init_velocity = sat1_init_velocity,
    init_rotation = 45
  )

  // случайная орбита с перигеем от 200 до 1000 км, и апогеем от 0 до 3000 км выше перигея
  val sat2_start_position = earth.coord + DVec(0, 1).rotateDeg(math.random * 360) * (earth.radius + 200000 + math.random * 800000)
  val sat2_init_velocity = speedToHaveOrbitWithParams(sat2_start_position, math.random * 3000000, earth.coord, earth.linearVelocity, earth.mass, G)

  // на круговой орбите в 200 км от поверхности Земли
  //val sat2_start_position = earth.coord + DVec(100, earth.radius + 199000)
  //val sat2_init_velocity = satelliteSpeed(sat2_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)/** 1.15 */

  println(s"sat2_start_position=$sat2_start_position")
  println(s"sat2_init_velocity=$sat2_init_velocity")
  val sat2 = new Satellite2(ScageId.nextId,
    init_coord = sat2_start_position,
    init_velocity = sat2_init_velocity,
    init_rotation = 0
  )

  // стоим на поверхности Земли
  val cargo1_start_position = earth.coord + DVec(0, earth.radius + 2)
  val cargo1_init_velocity = earth.linearVelocity + (cargo1_start_position - earth.coord).p * earth.groundSpeedMsec /*DVec.zero*/

  // на круговой орбите в 200 км от поверхности Земли
  //val cargo1_start_position = earth.coord + DVec(-100, earth.radius + 199000)
  //val cargo1_init_velocity = satelliteSpeed(cargo1_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)/** 1.15 */

  val cargo1 = new Cargo1(ScageId.nextId,
    init_coord = cargo1_start_position,
    init_velocity = cargo1_init_velocity,
    init_rotation = 0)

  val shipsHolder: ShipsHolder = new ShipsHolder(this)
}
