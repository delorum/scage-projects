package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A collider for circles hitting boxes, Circle = BodyA, Box = BodyB
 * <p/>
 * The create() method is used as a factory incase this class should
 * ever become stateful.
 *
 * @author Kevin Glass
 */
public strictfp class CircleBoxCollider extends BoxCircleCollider {
    /**
     * The single instance of this collider to exist
     */
    private static CircleBoxCollider single = new CircleBoxCollider();

    /**
     * Get an instance of this collider
     *
     * @return The instance of this collider
     */
    public static CircleBoxCollider createCircleBoxCollider() {
        return single;
    }

    /**
     * Prevent construction
     */
    private CircleBoxCollider() {
    }

    /**
     * @see Collider#collide(Contact[], Body, Body)
     */
    public int collide(Contact[] contacts, Body circleBody, Body boxBody) {
        int count = super.collide(contacts, boxBody, circleBody);

        // reverse the collision results by inverting normals
        // and projecting the results onto the circle
        for (int i = 0; i < count; i++) {
            Vector2f vec = MathUtil.scale(contacts[i].getNormal(), -1);
            contacts[i].setNormal(vec);

            Vector2f pt = MathUtil.sub(contacts[i].getPosition(), circleBody.getPosition());
            pt.normalise();
            pt.scale(((Circle) circleBody.getShape()).getRadius());
            pt.add(circleBody.getPosition());
            contacts[i].setPosition(pt);
        }

        return count;
    }
}