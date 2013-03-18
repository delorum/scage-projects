package net.scageprojects.drivecontrol

import net.scage.ScageLib.{drawCircle => scageDrawCircle, print => scagePrint, _}
import org.jbox2d.dynamics._
import org.jbox2d.common._
import org.jbox2d.collision.shapes.{CircleShape, PolygonShape}
import joints._
import org.jbox2d.callbacks.DebugDraw

sealed abstract class Shape
case class BoxShape(width:Float, height:Float, center:Vec = Vec.zero, angle_rad:Float = 0.0f, density:Float = 0f, friction:Float = 0.2f) extends Shape

class Physics2(val gravity:Vec = Vec.zero, val hz:Int = 60) {
  val dt = 1f/hz
  private val world = new World(new Vec2(gravity.x, gravity.y), true)
  private val debug_drawer = new ScageDrawer
  world.setDebugDraw(debug_drawer)

  def flags:Int = debug_drawer.getFlags
  def flags_=(new_flags:Int) {debug_drawer.setFlags(new_flags)}

  def staticBox(position:Vec, width:Float, height:Float, angleRad:Float = 0.0f):Body = {
    val fd: FixtureDef = new FixtureDef
    val sd: PolygonShape = new PolygonShape
    sd.setAsBox(width/2, height/2, new Vec2(0, 0), angleRad)

    fd.shape = sd
    val bd: BodyDef = new BodyDef
    bd.position = new Vec2(position.x, position.y)
    val body = world.createBody(bd)
    body.createFixture(fd)
    body
  }

  def staticEdge(from:Vec, to:Vec):Body = {
    val bd: BodyDef = new BodyDef
    val body: Body = world.createBody(bd)
    val shape: PolygonShape = new PolygonShape
    shape.setAsEdge(new Vec2(from.x, from.y), new Vec2(to.x, to.y))
    body.createFixture(shape, 0.0f)
    body
  }

  def dynamicBox(position:Vec, shape:BoxShape, angle_rad:Float = 0.0f, fixed_rotation:Boolean = false):Body = {
    dynamicBody(position, Seq(shape), angle_rad, fixed_rotation)
  }

  def dynamicBody(position:Vec, parts:Seq[Shape], angle_rad:Float = 0.0f, fixed_rotation:Boolean = false):Body = {
    val bd: BodyDef = new BodyDef
    bd.`type` = BodyType.DYNAMIC
    bd.position = new Vec2(position.x, position.y)
    bd.angle = angle_rad
    bd.fixedRotation = fixed_rotation
    val body = world.createBody(bd)

    parts.foreach {
      case BoxShape(width, height, center, local_angle_rad, density, friction) =>
        val fd: FixtureDef = new FixtureDef
        val sd: PolygonShape = new PolygonShape
        sd.setAsBox(width/2, height/2, new Vec2(center.x, center.y), local_angle_rad)
        fd.shape = sd
        fd.density = density
        fd.friction = friction
        body.createFixture(fd)
      case _ =>
    }

    body
  }

  def dynamicCircle(position:Vec, radius:Float, angleRad:Float = 0f, density:Float = 0f):Body = {
    val bd: BodyDef = new BodyDef
    bd.fixedRotation = true
    bd.position.set(new Vec2(position.x, position.y))
    bd.`type` = BodyType.DYNAMIC
    bd.angle = angleRad
    val body: Body = world.createBody(bd)
    val fd: FixtureDef = new FixtureDef
    val cd: CircleShape = new CircleShape
    cd.m_radius = radius
    fd.shape = cd
    fd.density = density
    //fd.filter.groupIndex = -2
    body.createFixture(fd)
    body
  }

  def constantVolumeJoint(frequencyHz:Float = 0.0f, dampingRatio:Float = 0.0f, bodies:Seq[Body]):Joint = {
    val cvjd = new ConstantVolumeJointDef
    cvjd.frequencyHz = frequencyHz
    cvjd.dampingRatio = dampingRatio
    bodies.foreach(b => cvjd.addBody(b))
    world.createJoint(cvjd)
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
  setFlags(DebugDraw.e_shapeBit | DebugDraw.e_jointBit)

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

  keyIgnorePause(KEY_F2, onKeyDown = restart())
  keyIgnorePause(KEY_W, 10, onKeyDown = c += Vec(0, 1f/globalScale))
  keyIgnorePause(KEY_A, 10, onKeyDown = c += Vec(-1f/globalScale, 0))
  keyIgnorePause(KEY_S, 10, onKeyDown = c += Vec(0, -1f/globalScale))
  keyIgnorePause(KEY_D, 10, onKeyDown = c += Vec(1f/globalScale, 0))
  keyIgnorePause(KEY_SPACE, onKeyDown = switchPause())

  mouseWheelUpIgnorePause(onWheelUp = m => globalScale += 1)
  mouseWheelDownIgnorePause(onWheelDown = m => if(globalScale > 0) globalScale -= 1)

  clear {
    physics.clearAll()
  }

  pause()
}

object Test1 extends TestBox("Test 1", Vec(0, -10)) {
  def initTest() {
    //floor
    physics.staticBox(position = Vec(0.0f, -10.0f), width = 50.0f*2, height = 10.0f*2)

    // platforms
    for(i <- 0 until 4) {
      physics.staticBox(position = Vec(0.0f, 5f + 5f * i), width = 15.0f*2, height = 0.125f*2)
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
      } physics.dynamicBox(position, shape = BoxShape(width = 0.125f*2, height = 2f*2, angle_rad = angle, density = 25.0f, friction = .5f))
    }
  }
}

object Test2 extends TestBox("Test 2", Vec(0, -10)) {
  def initTest() {
    physics.staticBox(position = Vec.zero, width = 50.0f*2, height = 0.4f*2)
    physics.staticBox(position = Vec(-10.0f, 0.0f), width = 0.4f*2, height = 50.0f*2)
    physics.staticBox(position = Vec(10.0f, 0.0f), width = 0.4f*2, height = 50.0f*2)

    val cx: Float = 0.0f
    val cy: Float = 10.0f
    val rx: Float = 5.0f
    val ry: Float = 5.0f
    val nBodies: Int = 20
    val bodyRadius: Float = 0.5f
    val bodies = for {
      i <- 0 until nBodies
      angle = MathUtils.map(i, 0, nBodies, 0, 2 * 3.1415f)
    } yield {
      physics.dynamicCircle(Vec(cx + rx*math.sin(angle), cy + ry*math.cos(angle)), bodyRadius, 1.0f)
    }
    physics.constantVolumeJoint(10.0f, 1.0f, bodies)

    physics.dynamicBox(position = Vec(cx, cy+15.0f), shape = BoxShape(width = 3.0f, height = 1.5f, density = 1.0f))
  }
}

object Test3 extends TestBox("Test 2", Vec(0, -10)) {
  def initTest() {
    physics.staticEdge(Vec(50, 0), Vec(-50, 0))

    for {
      i <- 0 until 10
      angle = MathUtils.randomFloat(-MathUtils.PI, MathUtils.PI)
      x = MathUtils.randomFloat(-0.1f, 0.1f)
      position = Vec(x + 5.0f, 1.05f + 2.5f*i)
    } physics.dynamicCircle(position, radius = 0.5f, angleRad = angle, density = 2.0f)

    for {
      i <- 0 until 10
      angle = MathUtils.randomFloat(-MathUtils.PI, MathUtils.PI)
      x = MathUtils.randomFloat(-0.1f, 0.1f)
      position = Vec(x - 5.0f, 1.05f + 2.5f*i)
    } physics.dynamicBody(position, parts = Seq(
      BoxShape(0.25f*2, 0.5f*2, density = 2.0f),
      BoxShape(0.25f*2, 0.5f*2, center = Vec(0.0f, -0.5f), angle_rad = 0.5f*MathUtils.PI, density = 2.0f)
    ), angle_rad = angle)

    physics.dynamicBody(Vec(0, 2), parts = Seq(
      BoxShape(1.5f*2, 0.15f*2, density = 4.0f),
      BoxShape(0.15f*2, 2.7f*2, center = Vec(-1.45f, 2.35f), angle_rad = 0.2f, density = 4.0f),
      BoxShape(0.15f*2, 2.7f*2, center = Vec(1.45f, 2.35f), angle_rad = -0.2f, density = 4.0f)
    ))
  }
}
