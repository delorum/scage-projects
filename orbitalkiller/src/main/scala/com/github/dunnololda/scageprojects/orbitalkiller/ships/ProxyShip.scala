package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageId
import com.github.dunnololda.scageprojects.orbitalkiller.{PolygonShape, DockingPoints, Engine, PolygonShip}

class ProxyShip(ship1:PolygonShip,
                ship2:PolygonShip,
                ship1_hull_points:List[DVec],
                ship2_hull_points:List[DVec],
                ship1_dp:DockingPoints,
                ship2_dp:DockingPoints) extends PolygonShip(
  ScageId.nextId,
  s"${ship1.name}-${ship2.name}",
  (ship1.mass*ship1.coord + ship2.mass*ship2.coord)/(ship1.mass + ship2.mass),
  (ship1.mass*ship1.linearVelocity + ship2.mass*ship2.linearVelocity)/(ship1.mass + ship2.mass),
  ship1.rotation, false, false) {

  lazy val ship1_coord_diff = ship1.coord - init_coord
  lazy val ship2_coord_diff = ship2.coord - init_coord
  lazy val ship2_rotation_diff = ship2.rotation - ship1.rotation
  
  def updateShipState(ship_index:Int): Unit = {
    if(ship_index == ship1.index) {
      ship1.currentState.coord = currentState.coord + ship1_coord_diff.rotateDeg(rotation)
      ship1.currentState.vel = currentState.vel
      ship1.currentState.ang = rotation
    } else if(ship_index == ship2.index) {
      ship2.currentState.coord = currentState.coord + ship2_coord_diff.rotateDeg(rotation)
      ship2.currentState.vel = currentState.vel
      ship2.currentState.ang = rotation  + ship2_rotation_diff
    }
  }

  def mass = ship1.mass + ship2.mass

  override val engines: List[Engine] = ship1.engines.filterNot(e => ship1_dp.disabled_engine.exists(_ == e.index)).map(e => {
    new Engine(e.index, e.position + ship1_coord_diff, e.force_dir, e.max_power, e.default_power_percent, e.fuel_consumption_per_sec_at_full_power, this)
  }) ::: ship2.engines.filterNot(e => ship2_dp.disabled_engine.exists(_ == e.index)).map(e => {
    new Engine(e.index + 10, e.position + ship2_coord_diff, e.force_dir, e.max_power, e.default_power_percent, e.fuel_consumption_per_sec_at_full_power, this)
  })

  override def fuelMass: Double = ship1.fuelMass + ship2.fuelMass

  override def fuelMass_=(m: Double): Unit = {}

  override lazy val convex_parts: List[PolygonShape] = {
    ship1.convex_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.convex_parts.map(p => p.copy(points = p.points.map(_ + ship2_coord_diff)))
  }

  /**
   * Точки, обозначающие корпус корабля. Задаются координатами относительно центра масс. Фактическую координату точки в данный момент времени
   * можно получить, повернув на текущий угол и прибавив координату ц.м.
   * @return
   */
  override lazy val points: List[DVec] = ship1_hull_points.map(_ + ship1_coord_diff) ::: ship2_hull_points.map(_ + ship2_coord_diff)

  override val wreck_parts: List[PolygonShape] = {
    ship1.wreck_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.wreck_parts.map(p => p.copy(points = p.points.map(_ + ship2_coord_diff)))
  }

  override lazy val engine_size: Double = math.min(ship1.engine_size, ship2.engine_size)

  override def preserveAngularVelocity(ang_vel_deg: Double): Unit = {}

  override def preserveVelocity(vel: DVec): Unit = {}

  override val is_manned: Boolean = ship1.is_manned || ship2.is_manned

  override val docking_points: List[DockingPoints] = {
    /*ship1.docking_points.filterNot(dp => ship1.dockData.exists(x => x.our_dp == dp || x.other_ship_dp == dp)).map(dp => {
      new Docking Pointsdp.p1 + ship1_coord_diff)
    }) :::
    ship2.docking_points.filterNot(dp => ship2.dockData.exists(x => x.our_dp == dp || x.other_ship_dp == dp))*/
    Nil
  }

  private val _keys_mapping = Map(
    1 -> KEY_NUMPAD1,
    2 -> KEY_NUMPAD2,
    3 -> KEY_NUMPAD3,
    4 -> KEY_NUMPAD4,
    6 -> KEY_NUMPAD6,
    7 -> KEY_NUMPAD7,
    8 -> KEY_NUMPAD8,
    9 -> KEY_NUMPAD9
  )

  override val engines_mapping: Map[Int, Engine] = if(ship1.is_manned) {
    engines.filter(_.index < 10).map(e => (_keys_mapping(e.index), e)).toMap
  } else if(ship2.is_manned) {
    engines.filter(_.index > 10).map(e => (_keys_mapping(e.index-10), e)).toMap
  } else Map()
  
  override protected def _afterStepConsumeFuel() {
    engines.foreach(e => {
      if (e.active) {
        if (e.workTimeTacts > 0) {
          e.workTimeTacts -= 1
          //InterfaceHolder.enginesInfo.addWorkTime(base_dt*1000*e.power/e.max_power)
          if(e.index < 10) {
            ship1.fuelMass -= e.fuelConsumptionPerTact
          } else {
            ship2.fuelMass -= e.fuelConsumptionPerTact
          }
        } else e.active = false
      }
    })
  }

  override def kill(reason: String, crash: Boolean): Unit = {
    ship1.kill(reason, crash)
    ship2.kill(reason, crash)
  }
}
