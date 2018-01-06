package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A simple Circle within the simulation, defined by its radius and the
 * position of the body to which it belongs
 *
 * @author Kevin Glass
 */
public strictfp class Circle extends AbstractShape implements DynamicShape {
    /**
     * The radius of the circle
     */
    private double radius;

    /**
     * Create a new circle based on its radius
     *
     * @param radius The radius of the circle
     */
    public Circle(double radius) {
        super(new AABox(radius * 2, radius * 2));

        this.radius = radius;
    }

    /**
     * Get the radius of the circle
     *
     * @return The radius of the circle
     */
    public double getRadius() {
        return radius;
    }

    /**
     * @see Shape#getSurfaceFactor()
     */
    public double getSurfaceFactor() {
        double circ = (double) (2 * Math.PI * radius);
        circ /= 2;

        return circ * circ;
    }

    /**
     * Check if this circle touches another
     *
     * @param x     The x position of this circle
     * @param y     The y position of this circle
     * @param other The other circle
     * @param ox    The other circle's x position
     * @param oy    The other circle's y position
     * @return True if they touch
     */
    public boolean touches(double x, double y, Circle other, double ox, double oy) {
        double totalRad2 = getRadius() + other.getRadius();

        if (Math.abs(ox - x) > totalRad2) {
            return false;
        }
        if (Math.abs(oy - y) > totalRad2) {
            return false;
        }

        totalRad2 *= totalRad2;

        double dx = Math.abs(ox - x);
        double dy = Math.abs(oy - y);

        return totalRad2 >= ((dx * dx) + (dy * dy));
    }
}
