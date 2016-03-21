package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib.{drawCircle => scageDrawCircle, _}
import org.jbox2d.dynamics._
import org.jbox2d.common._
import org.jbox2d.collision.shapes.{EdgeShape, CircleShape, PolygonShape}
import joints._
import org.jbox2d.callbacks.DebugDraw

object JBox2DTest extends ScageScreenApp("JBox2D Test", 640, 480) {
  val dt = 1f/63
  val world = new World(new Vec2(0, 0))
  private val debug_drawer = new ScageDrawer
  world.setDebugDraw(debug_drawer)

  val w = windowWidth/2
  val h = windowHeight/2

  def randomPos = Vec(w-95+math.random*190, h-95+math.random*190)
  def randomSpeed = Vec(-1 + math.random*2, -1 + math.random*2).n*50f

  def createWall(fromx:Float, fromy:Float, tox:Float, toy:Float) {
    val fd = new FixtureDef
    fd.restitution = 1
    fd.friction = 0
    val sd = new EdgeShape
    sd.set(new Vec2(fromx, fromy), new Vec2(tox, toy))
    fd.shape = sd
    val bd = new BodyDef
    val body = world.createBody(bd)
    body.createFixture(fd)
  }
  createWall(w - 100, h - 100, w + 100, h - 100)
  createWall(w - 100, h + 100, w + 100, h + 100)
  createWall(w - 100, h - 100, w - 100, h + 100)
  createWall(w + 100, h - 100, w + 100, h + 100)

  def createCircle(position:Vec, velocity:Vec, radius:Float) = {
    val fd = new FixtureDef
    fd.restitution = 1
    fd.friction = 0
    fd.density = 0.01f
    val sd = new CircleShape
    sd.m_radius = radius
    fd.shape = sd
    val bd = new BodyDef
    bd.`type` = BodyType.DYNAMIC
    bd.position = new Vec2(position.x, position.y)
    bd.linearVelocity = new Vec2(velocity.x, velocity.y)
    val body = world.createBody(bd)
    body.createFixture(fd)
    body
  }

  def createSquare(position:Vec, velocity:Vec, side:Float) = {
    val fd = new FixtureDef
    fd.restitution = 1
    fd.friction = 0
    fd.density = 1
    val sd = new PolygonShape
    sd.setAsBox(side/2, side/2)
    fd.shape = sd
    val bd = new BodyDef
    bd.`type` = BodyType.DYNAMIC
    bd.position = new Vec2(position.x, position.y)
    bd.linearVelocity = new Vec2(velocity.x, velocity.y)
    val body = world.createBody(bd)
    body.createFixture(fd)
    body
  }

  def createRect(position:Vec, velocity:Vec, side1:Float, side2:Float) = {
    val fd = new FixtureDef
    fd.restitution = 1
    fd.friction = 0
    fd.density = 1
    val sd = new PolygonShape
    sd.setAsBox(side1/2, side2/2)
    fd.shape = sd
    val bd = new BodyDef
    bd.`type` = BodyType.DYNAMIC
    bd.position = new Vec2(position.x, position.y)
    bd.angle = (45.0/180*math.Pi).toFloat
    bd.fixedRotation = false
    bd.linearVelocity = new Vec2(velocity.x, velocity.y)
    val body = world.createBody(bd)
    body.createFixture(fd)
    body
  }

  def test1() = {
    val c1 = createCircle(randomPos, randomSpeed, 10)
    val c2 = createCircle(randomPos, randomSpeed, 10)
    val c3 = createCircle(randomPos, randomSpeed, 10)
    val jd = new ConstantVolumeJointDef
    jd.addBody(c1)
    jd.addBody(c2)
    jd.addBody(c3)
    world.createJoint(jd)
    List(c1,c2,c3)
  }

  def test2() = {
    val c1 = createCircle(randomPos, randomSpeed, 10)
    val c2 = createCircle(randomPos, randomSpeed, 10)
    val jd = new DistanceJointDef
    jd.bodyA = c1
    jd.localAnchorA.set(new Vec2(0,10))
    jd.bodyB = c2
    jd.localAnchorB.set(new Vec2(0,10))
    jd.length = 50
    world.createJoint(jd)
    List(c1,c2)
  }

  def test3() = {
    val s1 = createSquare(randomPos, randomSpeed, 10)
    val s2 = createSquare(randomPos, randomSpeed, 10)
    val jd = new DistanceJointDef
    jd.bodyA = s1
    jd.localAnchorA.set(new Vec2(0,5))
    jd.bodyB = s2
    jd.localAnchorB.set(new Vec2(0,5))
    jd.length = 50
    world.createJoint(jd)
    List(s1,s2)
  }

  def test4() = {
    val s1 = createRect(randomPos, randomSpeed, 10, 70)
    List(s1)
  }

  def test5() = {
    val c1 = createCircle(randomPos, randomSpeed, 10)
    val c2 = createCircle(randomPos, randomSpeed, 10)
    val jd1 = new DistanceJointDef
    jd1.bodyA = c1
    jd1.localAnchorA.set(new Vec2(0,10))
    jd1.bodyB = c2
    jd1.localAnchorB.set(new Vec2(0,10))
    jd1.length = 50
    world.createJoint(jd1)
    val jd2 = new DistanceJointDef
    jd2.bodyA = c1
    jd2.localAnchorA.set(new Vec2(0,-10))
    jd2.bodyB = c2
    jd2.localAnchorB.set(new Vec2(0,-10))
    jd2.length = 50
    world.createJoint(jd2)
    List(c1,c2)
  }

  val bodies = test5()

  def energy = {
    bodies.map(b => {
      b.m_mass*b.m_linearVelocity.lengthSquared()/2 + b.m_I*b.m_angularVelocity*b.m_angularVelocity/2
    }).sum
  }

  private def nextStep() {
    world.step(dt, 8, 3)
  }
  nextStep()

  private var _center = windowCenter

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL)) stopApp())

  keyIgnorePause(KEY_W, 10, onKeyDown = {_center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {_center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {_center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {_center += Vec(5/globalScale, 0)})

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01f) {
      if(globalScale > 1) globalScale -= 1
      else if(globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
      if(globalScale < 0.01f) globalScale = 0.01f
    }
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 5) {
      if(globalScale < 1) globalScale += 0.1f
      else globalScale += 1
    }
  })

  action {
    nextStep()
  }

  center = _center

  render {
    world.drawDebugData()
  }

  interface {
    print(f"energy = $energy%.2f", 20, 20, WHITE)
  }
}

class ScageDrawer extends DebugDraw(new OBBViewportTransform) {
  setFlags(DebugDraw.e_shapeBit | DebugDraw.e_jointBit)

  override def drawSolidCircle(center: Vec2, radius: Float, axis: Vec2, color: Color3f): Unit = {
    drawFilledCircle(Vec(center.x, center.y), radius, ScageColor(color.x, color.y, color.z))
  }

  override def drawCircle(center: Vec2, radius: Float, color: Color3f): Unit = {
    scageDrawCircle(Vec(center.x, center.y), radius, ScageColor(color.x, color.y, color.z))
  }

  override def drawSegment(p1: Vec2, p2: Vec2, color: Color3f): Unit = {
    drawLine(Vec(p1.x, p1.y), Vec(p2.x, p2.y), ScageColor(color.x, color.y, color.z))
  }

  override def drawString(x: Float, y: Float, s: String, color: Color3f): Unit = {
    print(s, x, y, ScageColor(color.x, color.y, color.z), align = "center")
  }

  override def drawSolidPolygon(vertices: Array[Vec2], vertexCount: Int, color: Color3f): Unit = {
    vertexCount match {
      case 0 =>
      case 1 => scageDrawCircle(Vec(vertices(0).x, vertices(0).y), 3, ScageColor(color.x, color.y, color.z))
      case 2 => drawLine(Vec(vertices(0).x, vertices(0).y), Vec(vertices(1).x, vertices(1).y), ScageColor(color.x, color.y, color.z))
      case _ => drawFilledPolygon(vertices.take(vertexCount).map(v => Vec(v.x, v.y)), ScageColor(color.x, color.y, color.z))
    }
  }

  override def drawTransform(xf: Transform): Unit = {
    println("drawing transform...")
  }

  override def drawPoint(argPoint: Vec2, argRadiusOnScreen: Float, argColor: Color3f): Unit = {
    scageDrawCircle(Vec(argPoint.x, argPoint.y), argRadiusOnScreen, ScageColor(argColor.x, argColor.y, argColor.z))
  }
}