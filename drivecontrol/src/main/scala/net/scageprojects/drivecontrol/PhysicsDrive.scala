package net.scageprojects.drivecontrol

import net.scage.ScageLib.{drawCircle => scageDrawCircle, print => scagePrint, _}
import org.jbox2d.collision.AABB
import org.jbox2d.dynamics._
import org.jbox2d.common.{Transform, Color3f, OBBViewportTransform, Vec2}
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.dynamics.joints.{PrismaticJointDef, RevoluteJoint, RevoluteJointDef}
import org.jbox2d.callbacks.DebugDraw

class Physics2(val gravity:Vec = Vec.zero, val hz:Int = 60) {
  val dt = 1f/hz
  private val world = new World(new Vec2(gravity.x, gravity.y), true)
  private val debug_drawer = new ScageDrawer
  world.setDebugDraw(debug_drawer)

  def flags:Int = debug_drawer.getFlags
  def flags_=(new_flags:Int) = debug_drawer.setFlags(new_flags)

  def staticBox(position:Vec, width:Float, height:Float, angle:Float = 0.0f) {
    val fd: FixtureDef = new FixtureDef
    val sd: PolygonShape = new PolygonShape
    sd.setAsBox(width, height, new Vec2(position.x, position.y), angle)

    fd.shape = sd
    val bd: BodyDef = new BodyDef
    bd.position = new Vec2(position.x, position.y)
    world.createBody(bd).createFixture(fd)
  }

  def dynamicBox(position:Vec, angle:Float, width:Float, height:Float, density:Float, friction:Float) {
    val fd: FixtureDef = new FixtureDef
    val sd: PolygonShape = new PolygonShape
    sd.setAsBox(width, height)
    fd.shape = sd
    fd.density = density
    val bd: BodyDef = new BodyDef
    bd.`type` = BodyType.DYNAMIC
    fd.friction = friction
    bd.position = new Vec2(position.x, position.y)
    bd.angle = angle
    world.createBody(bd).createFixture(fd)
  }

  def step() {
    world.step(dt, 8, 3)
  }

  def drawDebugData() {
    world.drawDebugData()
  }

  def clearAll() {
    world.clearForces()
    var joint = world.getJointList
    while(joint != null) {
      world.destroyJoint(joint)
      joint = joint.getNext
    }
    var body = world.getBodyList
    while(body != null) {
      world.destroyBody(body)
      body = body.getNext
    }
  }
}

class ScageDrawer extends DebugDraw(new OBBViewportTransform) {
  setFlags(DebugDraw.e_shapeBit)

  def drawPoint(argPoint: Vec2, argRadiusOnScreen: Float, color: Color3f) {
    scageDrawCircle(Vec(argPoint.x, argPoint.y), argRadiusOnScreen, ScageColor(color.x, color.y, color.z))
  }

  def drawSolidPolygon(vertices: Array[Vec2], vertexCount: Int, color: Color3f) {
    drawFilledPolygon(vertices.take(vertexCount).map(v => Vec(v.x, v.y)), ScageColor(color.x, color.y, color.z))
  }

  def drawCircle(center: Vec2, radius: Float, color: Color3f) {
    scageDrawCircle(Vec(center.x, center.y), radius, ScageColor(color.x, color.y, color.z))
  }

  def drawSolidCircle(center: Vec2, radius: Float, axis: Vec2, color: Color3f) {
    drawFilledCircle(Vec(center.x, center.y), radius, ScageColor(color.x, color.y, color.z))
  }

  def drawSegment(p1: Vec2, p2: Vec2, color: Color3f) {
    drawLine(Vec(p1.x, p1.y), Vec(p2.x, p2.y), ScageColor(color.x, color.y, color.z))
  }

  def drawTransform(xf: Transform) {
    println("drawing transform...")
  }

  def drawString(x: Float, y: Float, s: String, color: Color3f) {
    scagePrint(s, x, y, WHITE, align = "center")
  }
}

abstract class TestBox(title:String, gravity:Vec) extends ScageScreenApp(title, 640, 480) {
  protected val physics = new Physics2(Vec(0, -10))

  def initTest()

  init {
    initTest()
  }

  action {
    physics.step()
  }

  private var c = Vec.zero
  center = c
  render {
    physics.drawDebugData()
  }

  key(KEY_F2, onKeyDown = restart())
  key(KEY_W, 10, onKeyDown = c += Vec(0, 1f/globalScale))
  key(KEY_A, 10, onKeyDown = c += Vec(-1f/globalScale, 0))
  key(KEY_S, 10, onKeyDown = c += Vec(0, -1f/globalScale))
  key(KEY_D, 10, onKeyDown = c += Vec(1f/globalScale, 0))

  mouseWheelUp(onWheelUp = m => globalScale += 1)
  mouseWheelDown(onWheelDown = m => if(globalScale > 0) globalScale -= 1)

  clear {
    physics.clearAll()
  }
}

object Test1 extends TestBox("Test 1", Vec(0, -10)) {
  def initTest() {
    //floor
    physics.staticBox(Vec(0.0f, -10.0f), 50.0f, 10.0f)

    // platforms
    for(i <- 0 until 4) {
      physics.staticBox(Vec(0.0f, 5f + 5f * i), 15.0f, 0.125f)
    }

    {
      val numPerRow: Int = 25
      for {
        i <- 0 until 4
        j <- 0 until numPerRow
        angle = if(i == 2 && j == 0) -0.1f else if(i == 3 && j == numPerRow - 1) .1f else 0f
        x = -14.75f + j * (29.5f / (numPerRow - 1))
        position = Vec(
          if(i == 2 && j == 0) x+0.1f else if(i == 3 && j == numPerRow - 1) x-0.1f else x,
          7.3f + 5f * i
        )
      } physics.dynamicBox(position, angle, width = 0.125f, height = 2f, density = 25.0f, friction = .5f)
    }
  }
}

object Test2 extends TestBox("Test 2", Vec(0, -10)) {
  def initTest() {
    physics.staticBox(Vec.zero, 50.0f, 0.4f)
    physics.staticBox(Vec(-10.0f, 0.0f), 0.4f, 50.0f)
    physics.staticBox(Vec(10.0f, 0.0f), 0.4f, 50.0f)
  }
}
