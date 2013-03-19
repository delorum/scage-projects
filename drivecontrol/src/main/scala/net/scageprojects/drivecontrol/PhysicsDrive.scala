package net.scageprojects.drivecontrol

import net.scage.ScageLib.{drawCircle => scageDrawCircle, print => scagePrint, _}
import org.jbox2d.dynamics._
import org.jbox2d.common._
import org.jbox2d.collision.shapes.{CircleShape, PolygonShape}
import joints._
import org.jbox2d.callbacks.DebugDraw

sealed abstract class ShapeDef
case class BoxShapeDef(width:Float,
                       height:Float,
                       center:Vec          = Vec.zero,
                       angle_rad:Float     = 0.0f,
                       density:Float       = 0f,
                       friction:Float      = 0.2f,
                       collision_group:Int = 0) extends ShapeDef
case class CircleShapeDef(radius:Float,
                          density:Float       = 0f,
                          friction:Float      = 0.2f,
                          collision_group:Int = 0) extends ShapeDef
case class EdgeShapeDef(from:Vec,
                        to:Vec,
                        collision_group:Int = 0) extends ShapeDef

class Physics2(val gravity:Vec = Vec.zero, val hz:Int = 60) {
  val dt = 1f/hz
  val world = new World(new Vec2(gravity.x, gravity.y), true)
  private val debug_drawer = new ScageDrawer
  world.setDebugDraw(debug_drawer)

  def flags:Int = debug_drawer.getFlags
  def flags_=(new_flags:Int) {debug_drawer.setFlags(new_flags)}

  def staticBody(position:Vec = Vec.zero, parts:Seq[ShapeDef], angle_rad:Float = 0.0f) = {
    val bd: BodyDef = new BodyDef
    bd.position = new Vec2(position.x, position.y)
    val body = world.createBody(bd)

    parts.foreach {
      case BoxShapeDef(width, height, center, local_angle_rad, _, _, collision_group) =>
        val fd: FixtureDef = new FixtureDef
        val sd: PolygonShape = new PolygonShape
        sd.setAsBox(width/2, height/2, new Vec2(center.x, center.y), local_angle_rad)
        fd.filter.groupIndex = collision_group
        fd.shape = sd
        /*fd.density = density
        fd.friction = friction*/
        body.createFixture(fd)
      case CircleShapeDef(radius, _, _, collision_group) =>
        val fd: FixtureDef = new FixtureDef
        fd.filter.groupIndex = collision_group
        val cd: CircleShape = new CircleShape
        cd.m_radius = radius
        fd.shape = cd
        /*fd.density = density
        fd.friction = friction*/
        //fd.filter.groupIndex = -2
        body.createFixture(fd)
      case EdgeShapeDef(from, to, collision_group) =>
        val fd: FixtureDef = new FixtureDef
        val sd: PolygonShape = new PolygonShape
        sd.setAsEdge(new Vec2(from.x, from.y), new Vec2(to.x, to.y))
        fd.shape = sd
        fd.filter.groupIndex = collision_group
        body.createFixture(fd)
    }

    body
  }

  def staticBox(position:Vec, shape:BoxShapeDef, angle_rad:Float = 0.0f):Body = {
    staticBody(position, Seq(shape), angle_rad)
  }

  def staticEdge(from:Vec, to:Vec):Body = {
    staticBody(parts = Seq(EdgeShapeDef(from, to)))
  }

  def dynamicBody(position:Vec,
                  parts:Seq[ShapeDef],
                  angle_rad:Float = 0.0f,
                  fixed_rotation:Boolean = false,
                  linear_damping:Float = 0f,
                  angular_damping:Float = 0f):Body = {
    val bd: BodyDef = new BodyDef
    bd.`type` = BodyType.DYNAMIC
    bd.position = new Vec2(position.x, position.y)
    bd.angle = angle_rad
    bd.fixedRotation = fixed_rotation
    bd.linearDamping = linear_damping
    bd.angularDamping = angular_damping
    val body = world.createBody(bd)

    parts.foreach {
      case BoxShapeDef(width, height, center, local_angle_rad, density, friction, collision_group) =>
        val fd: FixtureDef = new FixtureDef
        fd.filter.groupIndex = collision_group
        val sd: PolygonShape = new PolygonShape
        sd.setAsBox(width/2, height/2, new Vec2(center.x, center.y), local_angle_rad)
        fd.shape = sd
        fd.density = density
        fd.friction = friction
        body.createFixture(fd)
      case CircleShapeDef(radius, density, friction, collision_group) =>
        val fd: FixtureDef = new FixtureDef
        fd.filter.groupIndex = collision_group
        val cd: CircleShape = new CircleShape
        cd.m_radius = radius
        fd.shape = cd
        fd.density = density
        fd.friction = friction
        //fd.filter.groupIndex = -2
        body.createFixture(fd)
      case _ =>
    }

    body
  }

  def dynamicBox(position:Vec,
                 shape:BoxShapeDef,
                 angle_rad:Float = 0.0f,
                 fixed_rotation:Boolean = false,
                 linear_damping:Float = 0f,
                 angular_damping:Float = 0f):Body = {
    dynamicBody(position, Seq(shape), angle_rad, fixed_rotation)
  }

  def dynamicCircle(position:Vec,
                    shape:CircleShapeDef,
                    angle_rad:Float = 0f,
                    fixed_rotation:Boolean = false,
                    linear_damping:Float = 0f,
                    angular_damping:Float = 0f):Body = {
    dynamicBody(position, Seq(shape), angle_rad, fixed_rotation)
  }

  def constantVolumeJoint(frequencyHz:Float = 0.0f,
                          dampingRatio:Float = 0.0f,
                          bodies:Seq[Body]):ConstantVolumeJoint = {
    val cvjd = new ConstantVolumeJointDef
    cvjd.frequencyHz = frequencyHz
    cvjd.dampingRatio = dampingRatio
    bodies.foreach(b => cvjd.addBody(b))
    world.createJoint(cvjd).asInstanceOf[ConstantVolumeJoint]
  }

  def revoluteJoint(body1:Body,
                    body2:Body,
                    anchor:Vec,
                    collide_connected:Boolean = false,
                    motor_speed:Float = 0f,
                    max_motor_torque:Float = 0f,
                    enable_motor:Boolean = false):RevoluteJoint = {
    {
      val jd: RevoluteJointDef = new RevoluteJointDef
      jd.initialize(body1, body2, new Vec2(anchor.x, anchor.y))
      jd.collideConnected = false
      jd.motorSpeed = motor_speed
      jd.maxMotorTorque = max_motor_torque
      jd.enableMotor = enable_motor
      world.createJoint(jd).asInstanceOf[RevoluteJoint]
    }
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
    vertexCount match {
      case 0 =>
      case 1 => scageDrawCircle(Vec(vertices(0).x, vertices(0).y), 3, ScageColor(color.x, color.y, color.z))
      case 2 => drawLine(Vec(vertices(0).x, vertices(0).y), Vec(vertices(1).x, vertices(1).y), ScageColor(color.x, color.y, color.z))
      case _ => drawFilledPolygon(vertices.take(vertexCount).map(v => Vec(v.x, v.y)), ScageColor(color.x, color.y, color.z))
    }
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

abstract class TestBox(title:String, gravity:Vec = Vec(0, -10)) extends ScageScreenApp(title, 640, 480) {
  protected lazy val physics = new Physics2(gravity = gravity)

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

  interface {
    if(onPause) scagePrint("PAUSE", Vec(windowWidth/2, 20), DARK_GRAY, align = "center")
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

object Test1 extends TestBox("Domino") {
  def initTest() {
    //floor
    physics.staticBox(position = Vec(0.0f, -10.0f), shape = BoxShapeDef(width = 50.0f*2, height = 10.0f*2))

    // platforms
    for(i <- 0 until 4) {
      physics.staticBox(position = Vec(0.0f, 5f + 5f * i), shape = BoxShapeDef(width = 15.0f*2, height = 0.125f*2))
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
      } physics.dynamicBox(position, shape = BoxShapeDef(width = 0.125f*2, height = 2f*2, angle_rad = angle, density = 25.0f, friction = .5f))
    }
  }
}

object Test2 extends TestBox("Blob") {
  def initTest() {
    physics.staticBox(position = Vec.zero, shape = BoxShapeDef(width = 50.0f*2, height = 0.4f*2))
    physics.staticBox(position = Vec(-10.0f, 0.0f), shape = BoxShapeDef(width = 0.4f*2, height = 50.0f*2))
    physics.staticBox(position = Vec(10.0f, 0.0f), shape = BoxShapeDef(width = 0.4f*2, height = 50.0f*2))

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
      physics.dynamicCircle(Vec(cx + rx*math.sin(angle), cy + ry*math.cos(angle)), CircleShapeDef(bodyRadius, density = 1.0f, friction = 1.0f))
    }
    physics.constantVolumeJoint(10.0f, 1.0f, bodies)

    physics.dynamicBox(position = Vec(cx, cy+15.0f), shape = BoxShapeDef(width = 3.0f*2, height = 1.5f*2, density = 1.0f, friction = 1.0f))
  }
}

object Test3 extends TestBox("Shapes") {
  def initTest() {
    physics.staticEdge(Vec(50, 0), Vec(-50, 0))

    for {
      i <- 0 until 10
      angle = MathUtils.randomFloat(-MathUtils.PI, MathUtils.PI)
      x = MathUtils.randomFloat(-0.1f, 0.1f)
      position = Vec(x + 5.0f, 1.05f + 2.5f*i)
    } physics.dynamicCircle(position, CircleShapeDef(radius = 0.5f, density = 2.0f), angle_rad = angle)

    for {
      i <- 0 until 10
      angle = MathUtils.randomFloat(-MathUtils.PI, MathUtils.PI)
      x = MathUtils.randomFloat(-0.1f, 0.1f)
      position = Vec(x - 5.0f, 1.05f + 2.5f*i)
    } physics.dynamicBody(position, parts = Seq(
      BoxShapeDef(0.25f*2, 0.5f*2, density = 2.0f),
      BoxShapeDef(0.25f*2, 0.5f*2, center = Vec(0.0f, -0.5f), angle_rad = 0.5f*MathUtils.PI, density = 2.0f)
    ), angle_rad = angle)

    physics.dynamicBody(Vec(0, 2), parts = Seq(
      BoxShapeDef(1.5f*2, 0.15f*2, density = 4.0f),
      BoxShapeDef(0.15f*2, 2.7f*2, center = Vec(-1.45f, 2.35f), angle_rad = 0.2f, density = 4.0f),
      BoxShapeDef(0.15f*2, 2.7f*2, center = Vec(1.45f, 2.35f), angle_rad = -0.2f, density = 4.0f)
    ))
  }
}

object Test4 extends TestBox("TheoJansen") {
  def initTest() {
    val pivot = Vec(0.0f, 0.8f)
    val offset = Vec(0.0f, 8.0f)

    // ground
    physics.staticBody(parts = Seq(
      EdgeShapeDef(Vec(-50, 0), Vec(50, 0)),
      EdgeShapeDef(Vec(-50, 0), Vec(-50, 10)),
      EdgeShapeDef(Vec(50, 0), Vec(50, 10))
    ))

    // balls
    for(i <- 0 until 40) physics.dynamicCircle(Vec(-40+2*i, 0.5f), CircleShapeDef(0.25f, density = 1.0f))

    // chassis
    val chassis = physics.dynamicBox(position = pivot + offset, shape = BoxShapeDef(width = 2.5f*2, height = 1.0f*2, density = 1.0f, collision_group = -1))
    val wheel = physics.dynamicCircle(position = pivot + offset, shape = CircleShapeDef(radius = 1.6f, density = 1.0f, collision_group = -1))
    physics.revoluteJoint(chassis, wheel, pivot + offset, motor_speed = 2.0f, max_motor_torque = 400f, enable_motor = true)

    val wheel_anchor = pivot + Vec(0, -0.8f)
  }

  def createLeg(s:Float, wheel_anchor:Vec) {

  }
}

object Free extends TestBox(title = "Free", gravity = Vec.zero) {
  def initTest() {
    physics.dynamicBox(center, BoxShapeDef(50, 50, density = 1.0f))

  }

  val cannon_point = center + Vec(-windowWidth/2, -windowHeight/2)
  var angle = 45

  keyIgnorePause(KEY_LEFT, 10, onKeyDown = angle += 1)
  keyIgnorePause(KEY_RIGHT, 10, onKeyDown = angle -= 1)
  keyIgnorePause(KEY_LCONTROL, onKeyDown = {
    val b = physics.dynamicCircle(cannon_point + Vec(1,0).rotateDeg(angle)*40, CircleShapeDef(3, density = 1.0f))
    val dir = Vec(1,0).rotateDeg(angle)*40
    b.setLinearVelocity(new Vec2(dir.x, dir.y))
  })

  render {
    drawLine(cannon_point, cannon_point + Vec(1,0).rotateDeg(angle)*40, RED)
  }
}
