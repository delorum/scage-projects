package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller._

class ProxyShip(ship1:PolygonShip,
                ship1_init_coord:DVec,
                ship1_init_rotation:Double,
                ship1_dp:DockingPoints,
                ship2:PolygonShip,
                ship2_init_coord:DVec,
                ship2_init_rotation:Double,
                ship2_dp:DockingPoints) extends PolygonShip(
  ScageId.nextId,
  s"${ship1.name}-${ship2.name}",
  (ship1.mass*ship1.coord + ship2.mass*ship2.coord)/(ship1.mass + ship2.mass),
  (ship1.mass*ship1.linearVelocity + ship2.mass*ship2.linearVelocity)/(ship1.mass + ship2.mass),
  ship1.rotation, false, false) {

  println(s"init_rotation=$init_rotation")
  println(s"ship1_init_coord=$ship1_init_coord")
  println(s"ship2_init_coord=$ship2_init_coord")
  println(s"ship2_rotation_diff=$ship2_rotation_diff")
  println(s"init_coord=$init_coord")

  lazy val ship1_coord_diff = (ship1_init_coord - init_coord).rotateDeg(-init_rotation).map(v => DVec(v.x.round2Digits, v.y.round2Digits))
  lazy val ship2_rotation_diff = (ship2_init_rotation - ship1_init_rotation).round
  lazy val ship2_coord_diff = (ship2_init_coord - init_coord).rotateDeg(-init_rotation).map(v => DVec(v.x.round2Digits, v.y.round2Digits))

  def coordAndRotationDiff(ship_index:Int):(DVec, Double) = {
    val (our_coord_diff, our_rotation_diff) = ourCoordAndRotationDiff
    ship_index match {
      case ship1.index => (our_coord_diff + ship1_coord_diff, our_rotation_diff)
      case ship2.index => (our_coord_diff + ship2_coord_diff, our_rotation_diff + ship2_rotation_diff)
      case _ => (our_coord_diff, our_rotation_diff)
    }
  }

  override lazy val is_player:Boolean = ship1.is_player || ship2.is_player
  
  def updateShipState(ship_index:Int): Unit = {
    dock_data.foreach(_.proxy_ship.updateShipState(index))
    if(ship_index == ship1.index) {
      ship1.currentState.coord = currentState.coord + ship1_coord_diff.rotateDeg(rotation)
      ship1.currentState.vel = currentState.vel
      ship1.currentState.ang = rotation
      ship1.currentState.ang_vel = currentState.ang_vel
    } else if(ship_index == ship2.index) {
      ship2.currentState.coord = currentState.coord + ship2_coord_diff.rotateDeg(rotation)
      ship2.currentState.vel = currentState.vel
      ship2.currentState.ang = rotation  + ship2_rotation_diff
      ship2.currentState.ang_vel = currentState.ang_vel
    }
  }

  def mass = ship1.mass + ship2.mass

  override val engines: List[Engine] = ship1.engines ::: ship2.engines

  override def fuelMass: Double = ship1.fuelMass + ship2.fuelMass

  override def fuelMass_=(m: Double): Unit = {}

  override lazy val convex_parts: List[PolygonShape] = {
    ship1.convex_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.convex_parts.map(p => p.copy(points = p.points.map(p => p.rotateDeg(ship2_rotation_diff) + ship2_coord_diff)))
  }

  override def tryUndock:Boolean = ship1.tryUndock || ship2.tryUndock

  override def nearestFreeDockingPoints(coord:DVec):Option[DockingPoints] = {
    nonProxyShips.flatMap(s => s.docking_points.filter(dp => !s.dockData.exists(dd => dd.our_dp.index == dp.index))).sortBy(_.curP1.dist(coord)).headOption
  }

  /**
   * Точки, обозначающие корпус корабля. Задаются координатами относительно центра масс. Фактическую координату точки в данный момент времени
   * можно получить, повернув на текущий угол и прибавив координату ц.м.
   * @return
   */
  override lazy val points: List[DVec] = {
    ship1_dp.ordered_hull.map(_ + ship1_coord_diff) :::
    ship2_dp.ordered_hull.map(p => p.rotateDeg(ship2_rotation_diff) + ship2_coord_diff)
  }

  override val wreck_parts: List[PolygonShape] = {
    ship1.wreck_parts.map(p => p.copy(points = p.points.map(_ + ship1_coord_diff))) :::
    ship2.wreck_parts.map(p => p.copy(points = p.points.map(p => p.rotateDeg(ship2_rotation_diff) + ship2_coord_diff)))
  }

  override lazy val engine_size: Double = math.min(ship1.engine_size, ship2.engine_size)

  override def preserveAngularVelocity(ang_vel_deg: Double): Unit = {}

  override def preserveVelocity(vel: DVec): Unit = {}

  override val is_manned: Boolean = ship1.is_manned || ship2.is_manned

  override val engines_by_keycodes: Map[Int, Engine] = Map()

  override def consumeFuel() {
    ship1.consumeFuel()
    ship2.consumeFuel()
  }

  def nonProxyShips:List[PolygonShip] = {
    val x1 = ship1 match {
      case ps:ProxyShip => ps.nonProxyShips
      case s => List(s)
    }
    val x2 = ship2 match {
      case ps:ProxyShip => ps.nonProxyShips
      case s => List(s)
    }
    x1 ::: x2
  }

  override def checkDockingSituation(): Unit = {
    nonProxyShips.foreach(s => if(s.tryDock) s.dock())
    if(tryUndock) {
      ship1.undock()
    }
  }

  // TODO: направления двигателей ship2 могут меняться - зависят от ship2_rotation_diff. Но пока нет логики управления этими двигателями, не важно

  override def currentReactiveForce(time: Long, bs: BodyState): DVec = {
    ship1.currentReactiveForce(time, bs) +
    ship2.currentReactiveForce(time, bs)
  }

  override def currentReactiveForce(tacts: Long, bs: MutableBodyState): DVec = {
    ship1.currentReactiveForce(tacts, bs) +
    ship2.currentReactiveForce(tacts, bs)
  }

  override def currentTorque(time: Long): Double = {
    ship1.currentTorque(time) +
    ship2.currentTorque(time)
  }

  override def currentMass(time: Long): Double = {
    ship1.currentMass(time) +
    ship2.currentMass(time)
  }

  override def kill(reason: String, crash: Boolean): Unit = {
    ship1.kill(reason, crash)
    ship2.kill(reason, crash)
  }

  override def drawIfAliveBeforeRotation(): Unit = {
    if(!isDocked) drawFilledCircle(DVec.zero, 0.3, colorIfPlayerAliveOrRed(GREEN)) // mass center
    ship1.drawIfAliveBeforeRotation()
    ship2.drawIfAliveBeforeRotation()
  }
  override def drawIfAliveAfterRotation(): Unit = {
    InterfaceHolder.proxyHullSwitcher.selectedVariant match {
      case 0 =>
        ship1.drawIfAliveAfterRotation()
        ship2.drawIfAliveAfterRotation()
      case 1 =>
        drawSlidingLines(draw_points, GREEN)
      case 2 =>
        convex_parts.foreach(p => {
          drawSlidingLines(p.points ::: List(p.points.head), GREEN)
        })
      case 3 =>
        wreck_parts.foreach(p => {
          drawSlidingLines(p.points ::: List(p.points.head), GREEN)
        })
      case _ =>
        ship1.drawIfAliveAfterRotation()
        ship2.drawIfAliveAfterRotation()
    }
  }

  override val docking_points: List[DockingPoints] = Nil
}
