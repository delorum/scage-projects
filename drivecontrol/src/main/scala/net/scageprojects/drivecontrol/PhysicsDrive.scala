package net.scageprojects.drivecontrol

import net.scage.ScageLib._
import org.jbox2d.collision.AABB
import org.jbox2d.dynamics.{Body, BodyType, BodyDef, World}
import org.jbox2d.common.Vec2
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.dynamics.joints.{PrismaticJointDef, RevoluteJoint, RevoluteJointDef}

object PhysicsDrive extends ScageScreenApp("Physics Drive", 640, 480) {
  val MAX_STEER_ANGLE = math.Pi/3
  val STEER_SPEED = 1.5
  val SIDEWAYS_FRICTION_FORCE:Number = 10
  val HORSEPOWERS:Number = 40
  val CAR_STARTING_POS = new Vec2(10, 10)

  val leftRearWheelPosition = new Vec2(-1.5f, 1.90f)
  val rightRearWheelPosition = new Vec2(1.5f, 1.9f)
  val leftFrontWheelPosition = new Vec2(-1.5f, -1.9f)
  val rightFrontWheelPosition = new Vec2(1.5f, -1.9f)

  var engineSpeed = 0
  var steeringAngle = 0

  val worldBox = new AABB()
  worldBox.lowerBound.set(-100, -100)
  worldBox.upperBound.set(100,100)

  val myWorld = new World(new Vec2(0, 0), true)

  //Create some static stuff
  val staticDef = new BodyDef()
  staticDef.position.set(5, 20)
  val staticBox = new PolygonShape()
  staticBox.setAsBox(5, 5)
  myWorld.createBody(staticDef).createFixture(staticBox, 0)
  staticDef.position.x = 25
  myWorld.createBody(staticDef).createFixture(staticBox, 0)
  staticDef.position.set(15, 24)
  myWorld.createBody(staticDef).createFixture(staticBox, 0)

  // define our body
  val bodyDef = new BodyDef()
  bodyDef.`type` = BodyType.DYNAMIC
  bodyDef.linearDamping = 1
  bodyDef.angularDamping = 1
  bodyDef.position = CAR_STARTING_POS.clone()

  val body = myWorld.createBody(bodyDef)

  val leftWheelDef = new BodyDef()
  leftWheelDef.`type` = BodyType.DYNAMIC
  leftWheelDef.position = CAR_STARTING_POS.clone()
  leftWheelDef.position.add(leftFrontWheelPosition)
  val leftWheel = myWorld.createBody(leftWheelDef)

  val rightWheelDef = new BodyDef()
  rightWheelDef.`type` = BodyType.DYNAMIC
  rightWheelDef.position = CAR_STARTING_POS.clone()
  rightWheelDef.position.add(rightFrontWheelPosition)
  val rightWheel = myWorld.createBody(rightWheelDef)

  val leftRearWheelDef = new BodyDef()
  leftRearWheelDef.`type` = BodyType.DYNAMIC
  leftRearWheelDef.position = CAR_STARTING_POS.clone()
  leftRearWheelDef.position.add(leftRearWheelPosition)
  val leftRearWheel = myWorld.createBody(leftRearWheelDef)

  val rightRearWheelDef = new BodyDef()
  rightRearWheelDef.`type` = BodyType.DYNAMIC
  rightRearWheelDef.position = CAR_STARTING_POS.clone()
  rightRearWheelDef.position.add(rightRearWheelPosition)
  val rightRearWheel = myWorld.createBody(rightRearWheelDef)

  // define our shapes
  val boxDef:PolygonShape = new PolygonShape()
  boxDef.setAsBox(1.5f, 2.5f)
  body.createFixture(boxDef, 1)

  //Left Wheel shape
  val leftWheelShapeDef = new PolygonShape()
  leftWheelShapeDef.setAsBox(0.2f, 0.5f)
  leftWheel.createFixture(leftWheelShapeDef, 1)

  //Right Wheel shape
  val rightWheelShapeDef = new PolygonShape()
  rightWheelShapeDef.setAsBox(0.2f, 0.5f)
  rightWheel.createFixture(rightWheelShapeDef, 1)

  //Rear Left Wheel shape
  val leftRearWheelShapeDef = new PolygonShape()
  leftRearWheelShapeDef.setAsBox(0.2f, 0.5f)
  leftRearWheel.createFixture(leftRearWheelShapeDef, 1)

  //Rear Right Wheel shape
  val rightRearWheelShapeDef = new PolygonShape()
  rightRearWheelShapeDef.setAsBox(0.2f, 0.5f)
  rightRearWheel.createFixture(rightRearWheelShapeDef, 1)

  val leftJointDef = new RevoluteJointDef()
  leftJointDef.initialize(body, leftWheel, leftWheel.getWorldCenter)
  leftJointDef.enableMotor = true
  leftJointDef.maxMotorTorque = 100

  val rightJointDef = new RevoluteJointDef()
  rightJointDef.initialize(body, rightWheel, rightWheel.getWorldCenter)
  rightJointDef.enableMotor = true
  rightJointDef.maxMotorTorque = 100

  val leftJoint = new RevoluteJoint(myWorld.getPool, leftJointDef)
  val rightJoint = new RevoluteJoint(myWorld.getPool, rightJointDef)

  val leftRearJointDef = new PrismaticJointDef()
  leftRearJointDef.initialize(body, leftRearWheel, leftRearWheel.getWorldCenter, new Vec2(1, 0))
  leftRearJointDef.enableLimit = true
  leftRearJointDef.lowerTranslation = 0
  leftRearJointDef.upperTranslation = 0

  val rightRearJointDef = new PrismaticJointDef()
  rightRearJointDef.initialize(body, rightRearWheel, rightRearWheel.getWorldCenter, new Vec2(1, 0))
  rightRearJointDef.enableLimit = true
  rightRearJointDef.lowerTranslation = 0
  rightRearJointDef.upperTranslation = 0

  myWorld.createJoint(leftRearJointDef)
  myWorld.createJoint(rightRearJointDef)

  def killOrthogonalVelocity(targetBody:Body) {
    val localPoint = new Vec2(0, 0)
    val velocity = targetBody.getLinearVelocityFromLocalPoint(localPoint)

    val sidewaysAxis = new Vec2(1, 0)//targetBody.get.getXForm().R.col2.Copy()
    sidewaysAxis.mul(Vec2.dot(velocity, sidewaysAxis))

    targetBody.setLinearVelocity(sidewaysAxis) //targetBody.GetWorldPoint(localPoint));
  }

  action {
    myWorld.step(1.0f/30, 8, 8)
  }
}
