package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Collide a Convex Polygon with a Box.
 *
 * @author Gideon Smeding
 *
 */
public class PolygonBoxCollider extends PolygonPolygonCollider {

    /**
     * @see net.phys2d.raw.collide.Collider#collide(net.phys2d.raw.Contact[], net.phys2d.raw.Body, net.phys2d.raw.Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        Polygon poly = (Polygon) bodyA.getShape();
        Box box = (Box) bodyB.getShape();

        // TODO: this can be optimized using matrix multiplications and moving only one shape
        // specifically the box, because it has fewer vertices.
        Vector2f[] vertsA = poly.getVertices(bodyA.getPosition(), bodyA.getRotation());
        Vector2f[] vertsB = box.getPoints(bodyB.getPosition(), bodyB.getRotation());

        // TODO: use a sweepline that has the smallest projection of the box
        // now we use just an arbitrary one
        Vector2f sweepline = new Vector2f(vertsB[1]);
        sweepline.sub(vertsB[2]);

        EdgeSweep sweep = new EdgeSweep(sweepline);

        sweep.addVerticesToSweep(true, vertsA);
        sweep.addVerticesToSweep(false, vertsB);

        int[][] collEdgeCands = sweep.getOverlappingEdges();
//		FeaturePair[] featurePairs = getFeaturePairs(contacts.length, vertsA, vertsB, collEdgeCands);
//		return populateContacts(contacts, vertsA, vertsB, featurePairs);

        Intersection[][] intersections = getIntersectionPairs(vertsA, vertsB, collEdgeCands);
        return populateContacts(contacts, vertsA, vertsB, intersections);
    }

}
