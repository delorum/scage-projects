package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * The logic for checking lines against boxes
 *
 * @author Kevin Glass
 */
public strictfp class LineBoxCollider implements Collider {
    /**
     * The single instance of this class
     */
    private static LineBoxCollider single = new LineBoxCollider();

    /**
     * Create a new collider - place holder in case the collider
     * becomes stateful later.
     *
     * @return The new collider
     */
    public static LineBoxCollider create() {
        return single;
    }

    /**
     * Get the proportion that the src vector is of
     * the DENominator vector
     *
     * @param src The source vector
     * @param den The denominator vector
     * @return The proportion of the den that src is
     */
    private double getProp(Vector2f src, Vector2f den) {
        if ((den.getX() == 0) && (den.getY() == 0)) {
            return 0;
        }

        if (den.getX() != 0) {
            return src.getX() / den.getX();
        }

        return src.getY() / den.getY();
    }

    /**
     * @see Collider#collide(Contact[], Body, Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        int numContacts = 0;

        Line line = (Line) bodyA.getShape();
        Box box = (Box) bodyB.getShape();

        Vector2f lineVec = new Vector2f(line.getDX(), line.getDY());
        lineVec.normalise();
        Vector2f axis = new Vector2f(-line.getDY(), line.getDX());
        axis.normalise();

        Vector2f res = new Vector2f();
        line.getStart().projectOntoUnit(axis, res);
        double linePos = getProp(res, axis);

        Vector2f c = MathUtil.sub(bodyB.getPosition(), bodyA.getPosition());
        c.projectOntoUnit(axis, res);
        double centre = getProp(res, axis);

        Vector2f[] pts = box.getPoints(bodyB.getPosition(), bodyB.getRotation());
        double[] tangent = new double[4];
        double[] proj = new double[4];

        int outOfRange = 0;

        for (int i = 0; i < 4; i++) {
            pts[i].sub(bodyA.getPosition());
            pts[i].projectOntoUnit(axis, res);
            tangent[i] = getProp(res, axis);
            pts[i].projectOntoUnit(lineVec, res);
            proj[i] = getProp(res, new Vector2f(line.getDX(), line.getDY()));

            if ((proj[i] >= 1) || (proj[i] <= 0)) {
                outOfRange++;
            }
        }
        if (outOfRange == 4) {
            return 0;
        }

        Vector2f normal = new Vector2f(axis);

        if (centre < linePos) {
            if (!line.blocksInnerEdge()) {
                return 0;
            }

            normal.scale(-1);
            for (int i = 0; i < 4; i++) {
                if (tangent[i] > linePos) {
                    if (proj[i] < 0) {
                        Vector2f onAxis = new Vector2f();
                        Line leftLine = new Line(getPt(pts, i - 1), pts[i]);
                        Line rightLine = new Line(getPt(pts, i + 1), pts[i]);
                        leftLine.getClosestPoint(line.getStart(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double left = getProp(onAxis, axis);
                        rightLine.getClosestPoint(line.getStart(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double right = getProp(onAxis, axis);

                        if ((left > 0) && (right > 0)) {
                            Vector2f pos = new Vector2f(bodyA.getPosition());
                            pos.add(line.getStart());

                            resolveEndPointCollision(pos, bodyA, bodyB, normal, leftLine, rightLine, contacts[numContacts], i);
                            numContacts++;
                        }
                    } else if (proj[i] > 1) {
                        Vector2f onAxis = new Vector2f();
                        Line leftLine = new Line(getPt(pts, i - 1), pts[i]);
                        Line rightLine = new Line(getPt(pts, i + 1), pts[i]);
                        leftLine.getClosestPoint(line.getEnd(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double left = getProp(onAxis, axis);
                        rightLine.getClosestPoint(line.getEnd(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double right = getProp(onAxis, axis);

                        if ((left > 0) && (right > 0)) {
                            Vector2f pos = new Vector2f(bodyA.getPosition());
                            pos.add(line.getEnd());

                            resolveEndPointCollision(pos, bodyA, bodyB, normal, leftLine, rightLine, contacts[numContacts], i);
                            numContacts++;
                        }
                    } else {
                        pts[i].projectOntoUnit(lineVec, res);
                        res.add(bodyA.getPosition());
                        contacts[numContacts].setSeparation(-(tangent[i] - linePos));
                        contacts[numContacts].setPosition(new Vector2f(res));
                        contacts[numContacts].setNormal(normal);
                        contacts[numContacts].setFeature(new FeaturePair(i));
                        numContacts++;
                    }
                }
            }
        } else {
            if (!line.blocksOuterEdge()) {
                return 0;
            }

            for (int i = 0; i < 4; i++) {
                if (tangent[i] < linePos) {
                    if (proj[i] < 0) {
                        Vector2f onAxis = new Vector2f();
                        Line leftLine = new Line(getPt(pts, i - 1), pts[i]);
                        Line rightLine = new Line(getPt(pts, i + 1), pts[i]);
                        leftLine.getClosestPoint(line.getStart(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double left = getProp(onAxis, axis);
                        rightLine.getClosestPoint(line.getStart(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double right = getProp(onAxis, axis);

                        if ((left < 0) && (right < 0)) {
                            Vector2f pos = new Vector2f(bodyA.getPosition());
                            pos.add(line.getStart());

                            resolveEndPointCollision(pos, bodyA, bodyB, normal, leftLine, rightLine, contacts[numContacts], i);
                            numContacts++;
                        }
                    } else if (proj[i] > 1) {
                        Vector2f onAxis = new Vector2f();
                        Line leftLine = new Line(getPt(pts, i - 1), pts[i]);
                        Line rightLine = new Line(getPt(pts, i + 1), pts[i]);
                        leftLine.getClosestPoint(line.getEnd(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double left = getProp(onAxis, axis);
                        rightLine.getClosestPoint(line.getEnd(), res);
                        res.projectOntoUnit(axis, onAxis);
                        double right = getProp(onAxis, axis);

                        if ((left < 0) && (right < 0)) {
                            Vector2f pos = new Vector2f(bodyA.getPosition());
                            pos.add(line.getEnd());

                            resolveEndPointCollision(pos, bodyA, bodyB, normal, leftLine, rightLine, contacts[numContacts], i);
                            numContacts++;
                        }
                    } else {
                        pts[i].projectOntoUnit(lineVec, res);
                        res.add(bodyA.getPosition());
                        contacts[numContacts].setSeparation(-(linePos - tangent[i]));
                        contacts[numContacts].setPosition(new Vector2f(res));
                        contacts[numContacts].setNormal(normal);
                        contacts[numContacts].setFeature(new FeaturePair());
                        numContacts++;
                    }
                }
            }
        }

        if (numContacts > 2) {
            throw new RuntimeException("LineBoxCollision: > 2 contacts");
        }

        return numContacts;
    }

    /**
     * Resolve the collision math around an end point
     *
     * @param pos       The position of the contact
     * @param bodyA     The first body in the collision
     * @param bodyB     The second body in the collision
     * @param leftLine  The line to the left of the vertex of collision
     * @param rightLine The line to the right of the vertex of collision
     * @param contact   The contact to populate
     * @param norm      The normal determined for the line
     * @param i         The index of teh face we're resolving for feature ID
     */
    private void resolveEndPointCollision(Vector2f pos, Body bodyA, Body bodyB, Vector2f norm, Line leftLine, Line rightLine, Contact contact, int i) {
        Vector2f start = new Vector2f(pos);
        Vector2f end = new Vector2f(start);
        end.add(norm);

        rightLine.move(bodyA.getPosition());
        leftLine.move(bodyA.getPosition());
        Line normLine = new Line(start, end);
        Vector2f rightPoint = normLine.intersect(rightLine);
        Vector2f leftPoint = normLine.intersect(leftLine);

        double dis1 = Double.MAX_VALUE;
        if (rightPoint != null) {
            dis1 = rightPoint.distance(start) - norm.length();
        }
        double dis2 = Double.MAX_VALUE;
        if (leftPoint != null) {
            dis2 = leftPoint.distance(start) - norm.length();
        }

        norm.normalise();
        double dis = Math.min(dis1, dis2);

        contact.setSeparation(-dis);
        contact.setPosition(pos);
        contact.setNormal(norm);
        contact.setFeature(new FeaturePair(i));
    }

    /**
     * Get a specified point in the array using wrap round
     *
     * @param pts   The points array to access
     * @param index The index into the array to retrieve (negative and > length
     *              will be resolved)
     * @return The vector at the index requested
     */
    private Vector2f getPt(Vector2f[] pts, int index) {
        if (index < 0) {
            index += pts.length;
        }
        if (index >= pts.length) {
            index -= pts.length;
        }

        return pts[index];
    }
}
