package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A collider for circle 2 circle collisions
 * <p/>
 * The create() method is used as a factory just in case this
 * class becomes stateful eventually.
 *
 * @author Kevin Glass
 */
public strictfp class CircleCircleCollider implements Collider {
    /**
     * @see Collider#collide(Contact[], Body, Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        double x1 = bodyA.getPosition().getX();
        double y1 = bodyA.getPosition().getY();
        double x2 = bodyB.getPosition().getX();
        double y2 = bodyB.getPosition().getY();

        boolean touches = bodyA.getShape().getBounds().touches(x1, y1, bodyB.getShape().getBounds(), x2, y2);
        if (!touches) {
            return 0;
        }

        Circle circleA = (Circle) bodyA.getShape();
        Circle circleB = (Circle) bodyB.getShape();

        touches = circleA.touches(x1, y1, circleB, x2, y2);
        if (!touches) {
            return 0;
        }

        Vector2f normal = MathUtil.sub(bodyB.getPosition(), bodyA.getPosition());
        double sep = (circleA.getRadius() + circleB.getRadius()) - normal.length();

        normal.normalise();
        Vector2f pt = MathUtil.scale(normal, circleA.getRadius());
        pt.add(bodyA.getPosition());

        contacts[0].setSeparation(-sep);
        contacts[0].setPosition(pt);
        contacts[0].setNormal(normal);

        FeaturePair fp = new FeaturePair();
        contacts[0].setFeature(fp);

        return 1;
    }
}
