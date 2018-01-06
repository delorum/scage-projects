package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Collide a circle with a convex polygon
 *
 * @author Gideon Smeding
 */
public class PolygonCircleCollider extends PolygonPolygonCollider {

    /**
     * @see net.phys2d.raw.collide.Collider#collide(net.phys2d.raw.Contact[], net.phys2d.raw.Body, net.phys2d.raw.Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        Polygon polyA = (Polygon) bodyA.getShape();
        Circle circle = (Circle) bodyB.getShape();

        // TODO: this can be optimized using matrix multiplications and moving only the circle
        Vector2f[] vertsA = polyA.getVertices(bodyA.getPosition(), bodyA.getRotation());

        Vector2f centroidA = new Vector2f(polyA.getCentroid());
        centroidA.add(bodyA.getPosition());


        int[][] collPairs = getCollisionCandidates(vertsA, centroidA, circle.getRadius(), bodyB.getPosition());

        int noContacts = 0;
        for (int i = 0; i < collPairs.length; i++) {
            if (noContacts >= contacts.length)
                return contacts.length;

            Vector2f lineStartA = vertsA[collPairs[i][0]];
            Vector2f lineEndA = vertsA[(collPairs[i][0] + 1) % vertsA.length];
            Line line = new Line(lineStartA, lineEndA);

            double dis2 = line.distanceSquared(bodyB.getPosition());
            double r2 = circle.getRadius() * circle.getRadius();

            if (dis2 < r2) {
                Vector2f pt = new Vector2f();

                line.getClosestPoint(bodyB.getPosition(), pt);
                Vector2f normal = new Vector2f(bodyB.getPosition());
                normal.sub(pt);
                double sep = circle.getRadius() - normal.length();
                normal.normalise();

                contacts[noContacts].setSeparation(-sep);
                contacts[noContacts].setPosition(pt);
                contacts[noContacts].setNormal(normal);
                contacts[noContacts].setFeature(new FeaturePair());
                noContacts++;
            }
        }

        return noContacts;
    }

    /**
     * Get the edges from a list of vertices that can collide with the given circle.
     * This uses a sweepline algorithm which is only efficient if some assumptions
     * are indeed true. See CPolygonCPolygonCollider for more information.
     *
     * @param vertsA    The vertices of a polygon that is collided with a circle
     * @param centroid  The center of the polygon
     * @param radius    The radius of the circle
     * @param circlePos The position (center) of the circle
     * @return The list of edges that can collide with the circle
     */
    protected int[][] getCollisionCandidates(Vector2f[] vertsA, ROVector2f centroid, double radius, ROVector2f circlePos) {
        Vector2f sweepDir = new Vector2f(centroid);
        sweepDir.sub(circlePos);
        sweepDir.normalise(); //TODO: this normalization might not be necessary

        EdgeSweep sweep = new EdgeSweep(sweepDir);//vertsA[0], true, true, dist);

        sweep.addVerticesToSweep(true, vertsA);

        double circProj = circlePos.dot(sweepDir);

        sweep.insert(0, false, -radius + circProj);
        sweep.insert(0, false, radius + circProj);

        return sweep.getOverlappingEdges();
    }

}