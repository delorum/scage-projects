package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A simple box in the engine - defined by a width and height
 *
 * @author Kevin Glass
 */
public strictfp class Box extends AbstractShape implements DynamicShape {
    /** The size of the box */
    private Vector2f size = new Vector2f();

    /**
     * Create a box in the simulation 
     *
     * @param width The width of a box
     * @param height The hieght of the box
     */
    public Box(double width, double height) {
        super();

        size.set(width,height);
        bounds = new AABox(size.length(), size.length());
    }

    /**
     * Get the size of this box
     *
     * @return The size of this box
     */
    public ROVector2f getSize() {
        return size;
    }

    /**
     * @see Shape#getSurfaceFactor()
     */
    public double getSurfaceFactor() {
        double x = size.getX();
        double y = size.getY();

        return (x * x + y * y);
    }

    /**
     * Get the current positon of a set of points
     *
     * @param pos The centre of the box
     * @param rotation The rotation of the box
     * @return The points building up a box at this position and rotation
     */
    public Vector2f[] getPoints(ROVector2f pos, double rotation) {
        Matrix2f R = new Matrix2f(rotation);
        Vector2f[] pts = new Vector2f[4];
        ROVector2f h = MathUtil.scale(getSize(),0.5f);

        pts[0] = MathUtil.mul(R, new Vector2f(-h.getX(), -h.getY()));
        pts[0].add(pos);
        pts[1] = MathUtil.mul(R, new Vector2f(h.getX(), -h.getY()));
        pts[1].add(pos);
        pts[2] = MathUtil.mul(R, new Vector2f( h.getX(),  h.getY()));
        pts[2].add(pos);
        pts[3] = MathUtil.mul(R, new Vector2f(-h.getX(),  h.getY()));
        pts[3].add(pos);

        return pts;
    }
}
