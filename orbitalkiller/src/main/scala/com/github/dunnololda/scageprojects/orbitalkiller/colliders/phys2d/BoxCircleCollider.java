package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A collider for boxes hitting circles. Box = bodyA, Circle = bodyB
 *
 * The create() method is used as a factor although since this collider
 * is currently stateless a single instance is returned.
 *
 * @author Kevin Glass
 */
public strictfp class BoxCircleCollider implements Collider {
    /**
     * @see Collider#collide(Contact[], Body, Body)
     */
    public int collide(Contact[] contacts, Body boxBody, Body circleBody) {
        double x1 = boxBody.getPosition().getX();
        double y1 = boxBody.getPosition().getY();
        double x2 = circleBody.getPosition().getX();
        double y2 = circleBody.getPosition().getY();

        boolean touches = boxBody.getShape().getBounds().touches(x1,y1,circleBody.getShape().getBounds(),x2,y2);
        if (!touches) {
            return 0;
        }

        Box box = (Box) boxBody.getShape();
        Circle circle = (Circle) circleBody.getShape();

        Vector2f[] pts = box.getPoints(boxBody.getPosition(), boxBody.getRotation());
        Line[] lines = new Line[4];
        lines[0] = new Line(pts[0],pts[1]);
        lines[1] = new Line(pts[1],pts[2]);
        lines[2] = new Line(pts[2],pts[3]);
        lines[3] = new Line(pts[3],pts[0]);

        double r2 = circle.getRadius() * circle.getRadius();
        int closest = -1;
        double closestDistance = Double.MAX_VALUE;

        for (int i=0;i<4;i++) {
            double dis = lines[i].distanceSquared(circleBody.getPosition());
            if (dis < r2) {
                if (closestDistance > dis) {
                    closestDistance = dis;
                    closest = i;
                }
            }
        }

        if (closest > -1) {
            double dis = (double) Math.sqrt(closestDistance);
            contacts[0].setSeparation(dis - circle.getRadius());

            // this should really be where the edge and the line
            // between the two elements cross?
            Vector2f contactPoint = new Vector2f();
            lines[closest].getClosestPoint(circleBody.getPosition(), contactPoint);

            Vector2f normal = MathUtil.sub(circleBody.getPosition(), contactPoint);
            normal.normalise();
            contacts[0].setNormal(normal);
            contacts[0].setPosition(contactPoint);
            contacts[0].setFeature(new FeaturePair());

            return 1;
        }

        return 0;
    }

}
