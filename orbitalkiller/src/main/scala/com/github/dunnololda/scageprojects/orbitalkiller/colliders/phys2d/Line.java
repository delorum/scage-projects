package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Implemenation of a bunch of maths functions to do with lines. Note
 * that lines can't be used as dynamic shapes right now - also collision
 * with the end of a line is undefined.
 *
 * @author Kevin Glass
 */
public strictfp class Line extends AbstractShape implements DynamicShape {
    /**
     * The start point of the line
     */
    private ROVector2f start;
    /**
     * The end point of the line
     */
    private ROVector2f end;
    /**
     * The vector between the two points
     */
    private Vector2f vec;
    /**
     * The length of the line squared
     */
    private double lenSquared;

    /**
     * Temporary storage - declared globally to reduce GC
     */
    private Vector2f loc = new Vector2f(0, 0);
    /**
     * Temporary storage - declared globally to reduce GC
     */
    private Vector2f v = new Vector2f(0, 0);
    /**
     * Temporary storage - declared globally to reduce GC
     */
    private Vector2f v2 = new Vector2f(0, 0);
    /**
     * Temporary storage - declared globally to reduce GC
     */
    private Vector2f proj = new Vector2f(0, 0);

    /**
     * Temporary storage - declared globally to reduce GC
     */
    private Vector2f closest = new Vector2f(0, 0);
    /**
     * Temporary storage - declared globally to reduce GC
     */
    private Vector2f other = new Vector2f(0, 0);

    /**
     * True if this line blocks on the outer edge
     */
    private boolean outerEdge = true;
    /**
     * True if this line blocks on the inner edge
     */
    private boolean innerEdge = true;

    /**
     * Create a new line based on the origin and a single point
     *
     * @param x     The end point of the line
     * @param y     The end point of the line
     * @param inner True if this line blocks on it's inner edge
     * @param outer True if this line blocks on it's outer edge
     */
    public Line(double x, double y, boolean inner, boolean outer) {
        this(0, 0, x, y);

        setBlocksInnerEdge(inner);
        setBlocksOuterEdge(outer);
    }

    /**
     * Create a new line based on the origin and a single point
     *
     * @param x The end point of the line
     * @param y The end point of the line
     */
    public Line(double x, double y) {
        this(x, y, true, true);
    }

    /**
     * Create a new line based on two points
     *
     * @param x1 The x coordinate of the start point
     * @param y1 The y coordinate of the start point
     * @param x2 The x coordinate of the end point
     * @param y2 The y coordinate of the end point
     */
    public Line(double x1, double y1, double x2, double y2) {
        this(new Vector2f(x1, y1), new Vector2f(x2, y2));
    }

    /**
     * Create a new line based on two points
     *
     * @param start The start point
     * @param end   The end point
     */
    public Line(ROVector2f start, ROVector2f end) {
        super();

//		double width = Math.abs(end.getX()-start.getX());
//		double height = Math.abs(end.getY()-start.getY());
//		double xoffset = width/2;
//		double yoffset = height/2;
//		if (width < 10) {
//			width = 10;
//		}
//		if (height < 10) {
//			height = 50;
//		}
//		if (end.getY() < start.getY()) {
//			yoffset = -yoffset;
//		}
//		if (end.getX() < start.getX()) {
//			xoffset = -xoffset;
//		}
        //TODO: do this properly!
        double radius = Math.max(start.length(), end.length());
        bounds = new AABox(0, 0, radius * 2, radius * 2);

        set(start, end);
    }

    /**
     * Check if this line blocks the inner side (for want of a better term)
     *
     * @return True if this line blocks the inner side
     */
    public boolean blocksInnerEdge() {
        return innerEdge;
    }

    /**
     * Indicate if this line blocks on it's inner edge
     *
     * @param innerEdge True if this line blocks on it's inner edge
     */
    public void setBlocksInnerEdge(boolean innerEdge) {
        this.innerEdge = innerEdge;
    }

    /**
     * Check if this line blocks the outer side (for want of a better term)
     *
     * @return True if this line blocks the outer side
     */
    public boolean blocksOuterEdge() {
        return outerEdge;
    }

    /**
     * Indicate if this line blocks on it's outer edge
     *
     * @param outerEdge True if this line blocks on it's outer edge
     */
    public void setBlocksOuterEdge(boolean outerEdge) {
        this.outerEdge = outerEdge;
    }

    /**
     * Get the start point of the line
     *
     * @return The start point of the line
     */
    public ROVector2f getStart() {
        return start;
    }

    /**
     * Get the end point of the line
     *
     * @return The end point of the line
     */
    public ROVector2f getEnd() {
        return end;
    }

    /**
     * Find the length of the line
     *
     * @return The the length of the line
     */
    public double length() {
        return vec.length();
    }

    /**
     * Find the length of the line squared (cheaper and good for comparisons)
     *
     * @return The length of the line squared
     */
    public double lengthSquared() {
        return vec.lengthSquared();
    }

    /**
     * Configure the line
     *
     * @param start The start point of the line
     * @param end   The end point of the line
     */
    public void set(ROVector2f start, ROVector2f end) {
        this.start = start;
        this.end = end;

        vec = new Vector2f(end);
        vec.sub(start);

        lenSquared = vec.length();
        lenSquared *= lenSquared;
    }

    /**
     * Get the x direction of this line
     *
     * @return The x direction of this line
     */
    public double getDX() {
        return end.getX() - start.getX();
    }

    /**
     * Get the y direction of this line
     *
     * @return The y direction of this line
     */
    public double getDY() {
        return end.getY() - start.getY();
    }

    /**
     * Get the x coordinate of the start point
     *
     * @return The x coordinate of the start point
     */
    public double getX1() {
        return start.getX();
    }

    /**
     * Get the y coordinate of the start point
     *
     * @return The y coordinate of the start point
     */
    public double getY1() {
        return start.getY();
    }

    /**
     * Get the x coordinate of the end point
     *
     * @return The x coordinate of the end point
     */
    public double getX2() {
        return end.getX();
    }

    /**
     * Get the y coordinate of the end point
     *
     * @return The y coordinate of the end point
     */
    public double getY2() {
        return end.getY();
    }

    /**
     * Get the shortest distance from a point to this line
     *
     * @param point The point from which we want the distance
     * @return The distance from the line to the point
     */
    public double distance(ROVector2f point) {
        return (double) Math.sqrt(distanceSquared(point));
    }

    /**
     * Get the shortest distance squared from a point to this line
     *
     * @param point The point from which we want the distance
     * @return The distance squared from the line to the point
     */
    public double distanceSquared(ROVector2f point) {
        getClosestPoint(point, closest);
        closest.sub(point);

        double result = closest.lengthSquared();

        return result;
    }

    /**
     * Get the closest point on the line to a given point
     *
     * @param point  The point which we want to project
     * @param result The point on the line closest to the given point
     */
    public void getClosestPoint(ROVector2f point, Vector2f result) {
        loc.set(point);
        loc.sub(start);

        v.set(vec);
        v2.set(vec);
        v2.scale(-1);

        v.normalise();
        loc.projectOntoUnit(v, proj);
        if (proj.lengthSquared() > vec.lengthSquared()) {
            result.set(end);
            return;
        }
        proj.add(start);

        other.set(proj);
        other.sub(end);
        if (other.lengthSquared() > vec.lengthSquared()) {
            result.set(start);
            return;
        }

        result.set(proj);
        return;
    }

    /**
     * @see Shape#getSurfaceFactor()
     */
    public double getSurfaceFactor() {
        return lengthSquared() / 2;
    }

    /**
     * Get a line starting a x,y and ending offset from the current
     * end point. Curious huh?
     *
     * @param displacement The displacement of the line
     * @param rotation     The rotation of the line in radians
     * @return The newly created line
     */
    public Line getPositionedLine(ROVector2f displacement, double rotation) {
        Vector2f[] verts = getVertices(displacement, rotation);
        Line line = new Line(verts[0], verts[1]);

        return line;
    }

    /**
     * Return a translated and rotated line.
     *
     * @param displacement The displacement of the line
     * @param rotation     The rotation of the line in radians
     * @return The two endpoints of this line
     */
    public Vector2f[] getVertices(ROVector2f displacement, double rotation) {
        double cos = (double) Math.cos(rotation);
        double sin = (double) Math.sin(rotation);

        Vector2f[] endPoints = new Vector2f[2];
        endPoints[0] = new Vector2f(//getX1(), getY1());
                getX1() * cos - getY1() * sin,
                getY1() * cos + getX1() * sin);
        endPoints[0].add(displacement);
        endPoints[1] = new Vector2f(//getX2(), getY2());
                getX2() * cos - getY2() * sin,
                getY2() * cos + getX2() * sin);
        endPoints[1].add(displacement);

        return endPoints;
    }

    /**
     * Move this line a certain amount
     *
     * @param v The amount to move the line
     */
    public void move(ROVector2f v) {
        Vector2f temp = new Vector2f(start);
        temp.add(v);
        start = temp;
        temp = new Vector2f(end);
        temp.add(v);
        end = temp;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return "[Line " + start + "," + end + "]";
    }

    /**
     * Intersect this line with another
     *
     * @param other The other line we should intersect with
     * @return The intersection point or null if the lines are parallel
     */
    public Vector2f intersect(Line other) {
        double dx1 = end.getX() - start.getX();
        double dx2 = other.end.getX() - other.start.getX();
        double dy1 = end.getY() - start.getY();
        double dy2 = other.end.getY() - other.start.getY();
        double denom = (dy2 * dx1) - (dx2 * dy1);

        if (denom == 0) {
            return null;
        }

        double ua = (dx2 * (start.getY() - other.start.getY())) - (dy2 * (start.getX() - other.start.getX()));
        ua /= denom;
        double ub = (dx1 * (start.getY() - other.start.getY())) - (dy1 * (start.getX() - other.start.getX()));
        ub /= denom;

        double u = ua;

        double ix = start.getX() + (u * (end.getX() - start.getX()));
        double iy = start.getY() + (u * (end.getY() - start.getY()));

        return new Vector2f(ix, iy);
    }

}
