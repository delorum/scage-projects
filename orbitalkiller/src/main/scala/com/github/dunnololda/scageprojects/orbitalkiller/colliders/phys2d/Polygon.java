package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A polygon represented by a list of its vertices in counterclockwise
 * ordering. Note that the term 'counterclockwise' depends on the
 * orientation of the axes: if x points to the right and y points up,
 * the vertices are counter clockwise.
 * This means that on many displays the ordering of vertices will be
 * clockwise because the y axis is pointing down.
 *
 * TODO: the polygon is immutable but that could be changed
 *
 * @author Gideon Smeding
 *
 */
public class Polygon extends AbstractShape implements DynamicShape {

    /** The vertices of this polygon in counterclockwise order */
    protected Vector2f[] vertices;
    /** The total area of this polygon */
    protected double area;
    /** The center of mass of this polygon */
    protected Vector2f centroid;

    /** Construct the polygon with a list of vertices
     * sorted in counterclockwise order.
     * Note that all the vector values will be copied.
     *
     * Throws an exception when too few vertices (&lt;3) are supplied.
     * TODO: throw an exception when the vertices arent counterclockwise?
     *
     * @param vertices Vertices sorted in counterclockwise order
     */
    public Polygon(ROVector2f[] vertices) {
        if ( vertices.length < 3 )
            throw new IllegalArgumentException("A polygon can not have fewer than 3 edges!");

        this.vertices = new Vector2f[vertices.length];

        for ( int i = 0; i < vertices.length; i++ ) {
            this.vertices[i] = new Vector2f(vertices[i]);
        }


        double r = computeBoundingCircleRadius();
        this.bounds = new AABox(r*2,r*2);
        this.area = computeArea();
        this.centroid = computeCentroid();
    }

    /**
     * A constructor that allows for overloading without using
     * the public constructor. Does absolutely nothing.
     */
    protected Polygon() {}

    /**
     * Computes the area as described by Paul Borke.
     * See: http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/
     *
     * @return this polygon's computed area
     */
    protected double computeArea() {
        this.area = 0;

        Vector2f v1, v2;

        for ( int i = 0; i < vertices.length; i++ ) {
            v1 = vertices[i];
            v2 = vertices[(i+1) % vertices.length];

            this.area += v1.x * v2.y;
            this.area -= v2.x * v1.y;
        }

        return Math.abs(this.area / 2f);
    }

    /**
     * Compute the centroid (center of mass) as described by Paul Borke.
     * See: http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/
     *
     * Make sure you have computed the area before calling this!
     *
     * @return the computed centroid
     */
    protected Vector2f computeCentroid() {
        double x = 0;
        double y = 0;

        Vector2f v1, v2;

        for ( int i = 0; i < vertices.length; i++ ) {
            v1 = vertices[i];
            v2 = vertices[(i+1) % vertices.length];

            x += (v1.x + v2.x) * (v1.x * v2.y - v2.x * v1.y);
            y += (v1.y + v2.y) * (v1.x * v2.y - v2.x * v1.y);
        }

        return new Vector2f(x / (6 * this.area), y / (6 * this.area));
    }

    /**
     * Computes the radius of an approximation of a minimal bounding circle
     * which has its origin at (0,0) and sets this.bounds.
     *
     * TODO: this can be done much better
     *
     * @return The
     */
    protected double computeBoundingCircleRadius() {
        double r = 0;
        double l;

        for ( int i = 0; i < vertices.length; i++ ) {
            l = vertices[i].x * vertices[i].x + vertices[i].y * vertices[i].y;
            r = l > r ? l : r;
        }

        return (double) Math.sqrt(r);
    }

    /**
     * Get the area of this polygon
     * @return the area of this polygon
     */
    public double getArea() {
        return area;
    }

    /**
     * Get the center of mass (aka centroid) for this polygon.
     * @return the center of mass
     */
    public Vector2f getCentroid() {
        return centroid;
    }

    /**
     * Returns a copy of the list of vertices. The vertices are sorted
     * counterclockwise.
     *
     * @return this polygons vertices
     */
    public ROVector2f[] getVertices() {
        ROVector2f[] roVertices = new ROVector2f[vertices.length];

        for ( int i = 0; i < vertices.length; i++ )
            roVertices[i] = vertices[i];

        return roVertices;
    }

    /**
     * Check wether or not the polygon is convex.
     *
     * @return true iff this polygon is convex
     */
    public boolean isConvex() {
        // check if all angles are smaller or equal to 180 degrees
        int l = vertices.length;

        for ( int i = 0; i < vertices.length; i++ ) {
            Vector2f x = vertices[i];
            Vector2f y = vertices[(i+1)%l];
            Vector2f z = vertices[(i+2)%l];

            // does the 3d cross product point up or down?
            if ( (z.x-x.x)*(y.y-x.y)-(y.x-x.x)*(z.y-x.y) >= 0 )
                return false;
        }

        return true;
    }

    /**
     * Returns a translated and rotated copy of this poly's vertices.
     * The vertices are rotated before they are translated, i.e. they
     * are rotated around the origin (0,0).
     * The vertices are sorted counterclockwise.
     *
     * This function is typically used to get the vertices for a 
     * specific body, for example to collide it with another body
     * or draw it.
     *
     * @param displacement The displacement with wich all the 
     * @param rotation
     * @return this polygon's vertices translated and rotated
     */
    public Vector2f[] getVertices(ROVector2f displacement, double rotation) {
        Vector2f[] retVertices = new Vector2f[vertices.length];

        double cos = (double) Math.cos(rotation);
        double sin = (double) Math.sin(rotation);

        for ( int i = 0; i < vertices.length; i++ ) {
            double x = vertices[i].x * cos - vertices[i].y * sin;
            double y = vertices[i].y * cos + vertices[i].x * sin;
            x += displacement.getX();
            y += displacement.getY();

            retVertices[i] = new Vector2f(x, y);
        }

        return retVertices;
    }

    /**
     * Returns a translated and rotated copy of this poly's centroid.
     * The centroid is rotated before it is translated, i.e. it
     * is rotated around the origin (0,0).
     *
     * @param displacement The displacement with wich all the 
     * @param rotation
     * @return this polygon's vertices translated and rotated
     */
    public Vector2f getCentroid(ROVector2f displacement, double rotation) {
        double cos = (double) Math.cos(rotation);
        double sin = (double) Math.sin(rotation);

        return new Vector2f(
                centroid.x * cos - centroid.y * sin + displacement.getX(),
                centroid.y * cos + centroid.x * sin + displacement.getY());
    }

    /**
     * Test whether or not the point p is in this polygon in O(n),
     * where n is the number of vertices in this polygon.
     *
     * @param p The point to be tested for inclusion in this polygon
     * @return true iff the p is in this polygon (not on a border)
     */
    public boolean contains(ROVector2f p) {
        // TODO: implement this

        return false;
    }

    /**
     * Get point on this polygon's hull that is closest to p.
     *
     * TODO: make this thing return a negative value when it is contained in the polygon
     *
     * @param p The point to search the closest point for
     * @return the nearest point on this vertex' hull
     */
    public ROVector2f getNearestPoint(ROVector2f p) {
        // TODO: implement this

        return null;
    }

    /**
     * @see Shape#getSurfaceFactor()
     */
    public double getSurfaceFactor() {
        // TODO: return the real surface factor
        return getArea();
    }

}
