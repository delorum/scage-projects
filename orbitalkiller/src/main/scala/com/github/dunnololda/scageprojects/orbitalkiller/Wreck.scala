package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ObjectIndices.{earthIndex, moonIndex, sunIndex}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Shape.PolygonShape
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.{BodyState, MutableBodyState}

abstract class Wreck(
    mass: Double,
    init_coord: DVec,
    init_velocity: DVec,
    init_rotation: Double,
    points: List[DVec],
    val is_main: Boolean)
  extends CelestialsAware {
  val index: Int = ScageId.nextId
  private val draw_points = points :+ points.head

  def colorIfPlayerAliveOrRed(color: => ScageColor): ScageColor = if (Main.player_ship.isDead) RED else color

  val currentState: MutableBodyState = new MutableBodyState(
    BodyState(
      index = index,
      mass = mass,
      vel = init_velocity,
      coord = init_coord,
      ang = init_rotation,
      shape = PolygonShape(points, Nil),
      restitution = 0.8
    )
  )

  system_evolution.addBody(
    currentState,
    (_, helper) => {
      helper.gravityForceFromTo(sunIndex, index) +
        helper.gravityForceFromTo(earthIndex, index) +
        helper.gravityForceFromTo(moonIndex, index) +
        helper.funcOfArrayOrDVecZero(
          Array(index, earthIndex),
          l => {
            val bs = l(0)
            val e = l(1)
            // val other_ship_states = helper.bodyStates(ShipsHolder.shipIndicies.filterNot(_ == player_ship.index))
            earth.airResistance(bs, e, /*other_ship_states, */ 28, 0.5)
          }
        )
    },
    (_, _) => 0.0
  )

  val start_tact: Long = system_evolution.tacts

  def coord: DVec = currentState.coord

  def linearVelocity: DVec = currentState.vel

  def angularVelocity: Double = currentState.ang_vel

  def rotation: Double = currentState.ang

  val render_id: Int = render {
    if (
      !drawMapMode && coord.dist2(player_ship.coord) < 100000 * 100000 && !celestialsHelper.planets
        .exists(p => p._2.coord.dist2(coord) < p._2.radius2)
    ) {
      openglLocalTransform {
        openglMove(currentState.coord - base)
        /*mbs.contacts.foreach(x => {
        if(x.a.index.contains("part") && x.b.index.contains("part")) {
          drawFilledCircle(x.contact_point - mbs.coord, 0.3, YELLOW)
          drawLine(x.contact_point - mbs.coord, x.contact_point - mbs.coord + x.normal.n, YELLOW)
          drawCircle(x.contact_point - mbs.coord, x.separation, YELLOW)
        }
      })*/
        openglRotateDeg(currentState.ang)
        drawSlidingLines(draw_points, colorIfPlayerAliveOrRed(WHITE))
      }
    }
  }

  // val burn_dist = 30000.0 + math.random*10000 + earth.radius

  if (!is_main) {
    actionStaticPeriod(1000) {
      if (
        timeMultiplier > 10 || coord.dist2(player_ship.coord) > 100000L * 100000L || celestialsHelper.planets
          .exists(p => p._2.coord.dist2(coord) < p._2.radius2)
      ) {
        system_evolution.removeBodyByIndex(index)
        delOperation(render_id)
        deleteSelf()
      }
    }
  }
}
