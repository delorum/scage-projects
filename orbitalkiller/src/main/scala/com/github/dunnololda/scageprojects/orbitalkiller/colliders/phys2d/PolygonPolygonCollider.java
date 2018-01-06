package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Collision detection functions for colliding two polygons.
 *
 * @author Gideon Smeding
 */
public class PolygonPolygonCollider implements Collider {
    public Boolean isCCW(Vector2f[] vertices) {
        Vector2f c = new Vector2f();
        for (Vector2f v : vertices) {
            c.add(v);
        }
        c.scale(1.0 / vertices.length);
        Double sum = 0.0;
        for (int i = 0; i < vertices.length; i++) {
            Vector2f x = new Vector2f(vertices[i].getX(), vertices[i].getY());
            x.add(c.negate());
            if (i == 0) {
                Vector2f p1 = vertices[vertices.length - 1];
                Vector2f p2 = vertices[i];
                sum += (p2.getX() - p1.getX()) * (p2.getY() + p1.getY());
            } else {
                Vector2f p1 = vertices[i - 1];
                Vector2f p2 = vertices[i];
                sum += (p2.getX() - p1.getX()) * (p2.getY() + p1.getY());
            }
        }
        return vertices.length > 2 && sum < 0;
    }

    /**
     * @see Collider#collide(Contact[], Body, Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        Polygon polyA = (Polygon) bodyA.getShape();
        Polygon polyB = (Polygon) bodyB.getShape();

        Vector2f[] vertsA = polyA.getVertices(bodyA.getPosition(), bodyA.getRotation());
        /*if(!isCCW(vertsA)) {
            System.out.println("vertsA");
        }*/
        Vector2f[] vertsB = polyB.getVertices(bodyB.getPosition(), bodyB.getRotation());
        /*if(!isCCW(vertsB)) {
            System.out.println("vertsB");
        }*/

        Vector2f centroidA = new Vector2f(polyA.getCentroid());
        centroidA.add(bodyA.getPosition());
        Vector2f centroidB = new Vector2f(polyB.getCentroid());
        centroidB.add(bodyB.getPosition());

        int[][] collEdgeCands = getCollisionCandidates(vertsA, vertsB, centroidA, centroidB);
        Intersection[][] intersections = getIntersectionPairs(vertsA, vertsB, collEdgeCands);
        return populateContacts(contacts, vertsA, vertsB, intersections);
    }

    /**
     * Collides two polygons represented by two (already translated and rotated)
     * lists of vertices for both vertices.
     * This function will check for collisions between the supplied list of edge
     * pairs and find the edges where the two polygons intersect.
     *
     * @param vertsA        The rotated and translated vertices of the first polygon
     * @param vertsB        The rotated and translated vertices of the second polygon
     * @param collEdgeCands The edges of the two vertices that can collide. Expects the
     *                      same layout as returned by
     *                      {@link PolygonPolygonCollider#getCollisionCandidates(EdgeSweep, Vector2f[], Vector2f[])}
     * @return The points where the two polygons overlap, with for each overlapping
     * area the ingoing and outgoing edges in feature pairs.
     */
    public Intersection[][] getIntersectionPairs(Vector2f[] vertsA, Vector2f[] vertsB, int[][] collEdgeCands) {
        if (collEdgeCands.length == 0)
            return new Intersection[0][2];

        IntersectionGatherer fpl = new IntersectionGatherer(vertsA, vertsB);

        for (int i = 0; i < collEdgeCands.length; i++) {
            fpl.intersect(collEdgeCands[i][0], collEdgeCands[i][1]);
        }

        return fpl.getIntersectionPairs();
    }

    /**
     * Given a list of intersections, calculate the collision information and
     * set the contacts with that information.
     *
     * @param contacts      The array of contacts to fill
     * @param vertsA        The vertices of polygon A
     * @param vertsB        The vertices of polygon B
     * @param intersections The array of intersections as returned by
     *                      {@link PolygonPolygonCollider#getIntersectionPairs(Vector2f[], Vector2f[], int[][])}
     * @return The number of contacts that have been determined and hence
     * populated in the array.
     */
    public int populateContacts(Contact[] contacts, Vector2f[] vertsA, Vector2f[] vertsB, Intersection[][] intersections) {
        if (intersections.length == 0)
            return 0;

        int noContacts = 0;

        for (int i = 0; i < intersections.length; i++) {
            if (noContacts >= contacts.length)
                return contacts.length;

            if (intersections[i].length == 2 && noContacts < contacts.length - 1) {
                setContactPair(
                        contacts[noContacts],
                        contacts[noContacts + 1],
                        intersections[i][0],
                        intersections[i][1],
                        vertsA, vertsB);

                noContacts += 2;
            } else if (intersections[i].length == 1) {
                setContact(contacts[noContacts], intersections[i][0], vertsA, vertsB);
                noContacts += 1;
            }
        }

        return noContacts;
    }

    /**
     * Set a single contact for an intersection. This is used when a contact wasn't
     * be paired up with another. Because we know that this only happens to very
     * shallow penetrations, it is safe to use a separation of 0.
     *
     * @param contact      The contact to be set
     * @param intersection The intection to set the contact information for
     * @param vertsA       The vertices of polygon A
     * @param vertsB       The vertices of polygon B
     */
    public void setContact(Contact contact, Intersection intersection, Vector2f[] vertsA, Vector2f[] vertsB) {
        Vector2f startA = vertsA[intersection.edgeA];
        Vector2f endA = vertsA[(intersection.edgeA + 1) % vertsA.length];
        Vector2f startB = vertsB[intersection.edgeB];
        Vector2f endB = vertsB[(intersection.edgeB + 1) % vertsB.length];

        Vector2f normal = MathUtil.getNormal(startA, endA);
        normal.sub(MathUtil.getNormal(startB, endB));
        normal.normalise();

        contact.setNormal(normal);
        contact.setSeparation(0);
        contact.setFeature(new FeaturePair(intersection.edgeA, intersection.edgeB, 0, 0));
        contact.setPosition(intersection.position);
    }

    /**
     * Calculate the collision normal and penetration depth of one intersection pair
     * and set two contacts.
     *
     * @param contact1 The first contact to be set
     * @param contact2 The first contact to be set
     * @param in       The ingoing intersection of the pair
     * @param out      The outgoing intersection of the pair
     * @param vertsA   The vertices of polygon A
     * @param vertsB   The vertices of polygon B
     */
    public void setContactPair(
            Contact contact1,
            Contact contact2,
            Intersection in,
            Intersection out,
            Vector2f[] vertsA,
            Vector2f[] vertsB) {
        Vector2f entryPoint = in.position;
        Vector2f exitPoint = out.position;

        Vector2f normal = MathUtil.getNormal(entryPoint, exitPoint);

        FeaturePair feature = new FeaturePair(in.edgeA, in.edgeB, out.edgeA, out.edgeB);

        double separation = -PenetrationSweep.getPenetrationDepth(in, out, normal, vertsA, vertsB);
        // divided by 2 because there are two contact points
        // divided by 2 (again) because both objects move (I think)
        separation /= 4;

        contact1.setSeparation(separation);
        contact1.setNormal(normal);
        contact1.setPosition(entryPoint);
        contact1.setFeature(feature);

        contact2.setSeparation(separation);
        contact2.setNormal(normal);
        contact2.setPosition(exitPoint);
        contact2.setFeature(feature);
    }


    /**
     * This function finds pairs of edges of two polygons by projecting all the
     * edges on a line. Essentially this is just an optimization to minimize the
     * number of line-line intersections that is tested for collisions.
     *
     * @param sweep  The sweepline object to use, this allows a user of this function to add other vertices
     * @param vertsA The vertices of the first polygon ordered counterclockwise (TODO: verify this/order matters?)
     * @param vertsB The vertices of the second polygon ordered counterclockwise
     * @return The pairs of vertices that overlap in the sweepline and are therefore collision candidates.
     * The returned array is of a shape int[n][2] where n is the number of overlapping edges.
     * For a returned array r
     * the edge between vertsA[r[x][0]] and vertsA[r[x][0] + 1]
     * overlaps with vertsB[r[x][1]] and vertsB[r[x][1] + 1].
     */
    public int[][] getCollisionCandidates(EdgeSweep sweep, Vector2f[] vertsA, Vector2f[] vertsB) {
        sweep.addVerticesToSweep(true, vertsA);
        sweep.addVerticesToSweep(false, vertsB);

        return sweep.getOverlappingEdges();
    }

    /**
     * This function finds pairs of edges of two polygons by projecting all the
     * edges on a line. Essentially this is just an optimization to minimize the
     * number of line-line intersections that is tested for collisions.
     * <p/>
     * This version simply calls
     * {@link PolygonPolygonCollider#getCollisionCandidates(EdgeSweep, Vector2f[], Vector2f[]) }
     * with a new empty EdgeSweep.
     *
     * @param vertsA        The vertices of the first polygon ordered counterclockwise (TODO: verify this/order matters?)
     * @param sweepDirStart The 'real' center of the first polygon
     * @param vertsB        The vertices of the second polygon ordered counterclockwise
     * @param sweepDirEnd   The 'real' center of the second polygon
     * @return The pairs of vertices that overlap in the sweepline and are therefore collision candidates.
     * The returned array is of a shape int[n][2] where n is the number of overlapping edges.
     * For a returned array r
     * the edge between vertsA[r[x][0]] and vertsA[r[x][0] + 1]
     * overlaps with vertsB[r[x][1]] and vertsB[r[x][1] + 1].
     */
    public int[][] getCollisionCandidates(Vector2f[] vertsA, Vector2f[] vertsB, Vector2f sweepDirStart, Vector2f sweepDirEnd) {
        Vector2f sweepDir = new Vector2f(sweepDirEnd);
        sweepDir.sub(sweepDirStart);

        return getCollisionCandidates(new EdgeSweep(sweepDir), vertsA, vertsB);
    }
}
