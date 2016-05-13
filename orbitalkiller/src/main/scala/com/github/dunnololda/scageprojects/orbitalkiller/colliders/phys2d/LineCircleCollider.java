package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Collision routines betwene a circle and a line. The create method is
 * provided in case this collider becomes stateful at some point.
 *
 * @author Kevin Glass
 */
public strictfp class LineCircleCollider implements Collider {

    /**
     * @see Collider#collide(Contact[], Body, Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        Line line = (Line) bodyA.getShape();
        Circle circle = (Circle) bodyB.getShape();

        Vector2f[] vertsA = line.getVertices(bodyA.getPosition(), bodyA.getRotation());

        // compute intersection of the line A and a line parallel to
        // the line A's normal passing through the origin of B
        Vector2f startA = vertsA[0];
        Vector2f endA = vertsA[1];
        ROVector2f startB = bodyB.getPosition();
        Vector2f endB = new Vector2f(endA);
        endB.sub(startA);
        endB.set(endB.y, -endB.x);
//		endB.add(startB);// TODO: inline endB into equations below, this last operation will be useless..

        //TODO: reuse mathutil.intersect
//		double d = (endB.y - startB.getY()) * (endA.x - startA.x);
//		d -= (endB.x - startB.getX()) * (endA.y - startA.y);
//
//		double uA = (endB.x - startB.getX()) * (startA.y - startB.getY());
//		uA -= (endB.y - startB.getY()) * (startA.x - startB.getX());
//		uA /= d;
        double d = endB.y * (endA.x - startA.x);
        d -= endB.x * (endA.y - startA.y);

        double uA = endB.x * (startA.y - startB.getY());
        uA -= endB.y * (startA.x - startB.getX());
        uA /= d;

        Vector2f position = null;

        if (uA < 0) { // the intersection is somewhere before startA
            position = startA;
        } else if (uA > 1) { // the intersection is somewhere after endA
            position = endA;
        } else {
            position = new Vector2f(
                    startA.x + uA * (endA.x - startA.x),
                    startA.y + uA * (endA.y - startA.y));
        }

        Vector2f normal = endB; // reuse of vector object
        normal.set(startB);
        normal.sub(position);
        double distSquared = normal.lengthSquared();
        double radiusSquared = circle.getRadius() * circle.getRadius();

        if (distSquared < radiusSquared) {
            contacts[0].setPosition(position);
            contacts[0].setFeature(new FeaturePair());

            normal.normalise();
            contacts[0].setNormal(normal);

            double separation = (double) Math.sqrt(distSquared) - circle.getRadius();
            contacts[0].setSeparation(separation);

            return 1;
        }

        return 0;
    }

}
