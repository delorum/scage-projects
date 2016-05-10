package com.github.dunnololda.scageprojects.orbitalkiller

import OrbitalKiller._
import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageId}

class Wreck(mass:Double, init_coord:DVec, init_velocity:DVec, init_rotation:Double, points:List[DVec], val is_main:Boolean) {
  val index = ScageId.nextId
  private val draw_points = points :+ points.head
  def colorIfPlayerAliveOrRed(color: => ScageColor) = if(OrbitalKiller.player_ship.isDead) RED else color

  val currentState = new MutableBodyState(BodyState(
    index = index,
    mass = mass,
    vel = init_velocity,
    coord = init_coord,
    ang = init_rotation,
    shape = PolygonShape(points, Nil),
    is_static = false,
    restitution = 0.8
  ))

  system_evolution.addBody(currentState,
    (tacts, helper) => {
      helper.gravityForceFromTo(sun.index, index) +
        helper.gravityForceFromTo(earth.index, index) +
        helper.gravityForceFromTo(moon.index, index) +
        helper.funcOfArrayOrDVecZero(Array(index, earth.index), l => {
          val bs = l(0)
          val e = l(1)
          val other_ship_states = helper.bodyStates(ShipsHolder.shipIndicies.filterNot(_ == player_ship.index))
          earth.airResistance(bs, e, other_ship_states, 28, 0.5)
        })
    },
    (tacts, helper) => 0.0
  )

  val start_tact = system_evolution.tacts
  
  def coord = currentState.coord
  def linearVelocity = currentState.vel
  def angularVelocity = currentState.ang_vel
  def rotation = currentState.ang

  val render_id = render {
    if(!drawMapMode && coord.dist2(player_ship.coord) < 100000*100000) {
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

  //val burn_dist = 30000.0 + math.random*10000 + earth.radius

  if(!is_main) {
    actionStaticPeriod(1000) {
      if (timeMultiplier > 10 || coord.dist2(player_ship.coord) > 100000l * 100000l) {
        system_evolution.removeBodyByIndex(index)
        delOperation(render_id)
        deleteSelf()
      }
    }
  }
}
