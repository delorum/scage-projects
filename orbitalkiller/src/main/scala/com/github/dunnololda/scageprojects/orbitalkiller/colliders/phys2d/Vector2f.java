package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

import com.github.dunnololda.scage.support.DVec;

/**
 * A two dimensional vector
 *
 * @author Kevin Glass
 */
public strictfp class Vector2f implements ROVector2f {
    /**
     * The x component of this vector
     */
    public double x;
    /**
     * The y component of this vector
     */
    public double y;

    /**
     * Create an empty vector
     */
    public Vector2f() {
    }

    /**
     * @see ROVector2f#getX()
     */
    public double getX() {
        return x;
    }

    /**
     * @see ROVector2f#getY()
     */
    public double getY() {
        return y;
    }

    /**
     * Create a new vector based on another
     *
     * @param other The other vector to copy into this one
     */
    public Vector2f(ROVector2f other) {
        this(other.getX(), other.getY());
    }

    /**
     * Create a new vector
     *
     * @param x The x component to assign
     * @param y The y component to assign
     */
    public Vector2f(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Set the value of this vector
     *
     * @param other The values to set into the vector
     */
    public void set(ROVector2f other) {
        set(other.getX(), other.getY());
    }

    /**
     * @see ROVector2f#dot(ROVector2f)
     */
    public double dot(ROVector2f other) {
        return (x * other.getX()) + (y * other.getY());
    }

    /**
     * Set the values in this vector
     *
     * @param x The x component to set
     * @param y The y component to set
     */
    public void set(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Negate this vector
     *
     * @return A copy of this vector negated
     */
    public Vector2f negate() {
        return new Vector2f(-x, -y);
    }

    /**
     * Add a vector to this vector
     *
     * @param v The vector to add
     */
    public void add(ROVector2f v) {
        x += v.getX();
        y += v.getY();
    }

    /**
     * Subtract a vector from this vector
     *
     * @param v The vector subtract
     */
    public void sub(ROVector2f v) {
        x -= v.getX();
        y -= v.getY();
    }

    /**
     * Scale this vector by a value
     *
     * @param a The value to scale this vector by
     */
    public void scale(double a) {
        x *= a;
        y *= a;
    }

    /**
     * Normalise the vector
     */
    public void normalise() {
        double l = length();

        if (l == 0)
            return;

        x /= l;
        y /= l;
    }

    /**
     * The length of the vector squared
     *
     * @return The length of the vector squared
     */
    public double lengthSquared() {
        return (x * x) + (y * y);
    }

    /**
     * @see ROVector2f#length()
     */
    public double length() {
        return (double) Math.sqrt(lengthSquared());
    }

    /**
     * Project this vector onto another
     *
     * @param b      The vector to project onto
     * @param result The projected vector
     */
    public void projectOntoUnit(ROVector2f b, Vector2f result) {
        double dp = b.dot(this);

        result.x = dp * b.getX();
        result.y = dp * b.getY();
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return "[Vec " + x + "," + y + " (" + length() + ")]";
    }

    /**
     * Get the distance from this point to another
     *
     * @param other The other point we're measuring to
     * @return The distance to the other point
     */
    public double distance(ROVector2f other) {
        return (double) Math.sqrt(distanceSquared(other));
    }

    /**
     * Get the distance squared from this point to another
     *
     * @param other The other point we're measuring to
     * @return The distance to the other point
     */
    public double distanceSquared(ROVector2f other) {
        double dx = other.getX() - getX();
        double dy = other.getY() - getY();

        return (dx * dx) + (dy * dy);
    }

    /**
     * Compare two vectors allowing for a (small) error as indicated by the delta.
     * Note that the delta is used for the vector's components separately, i.e.
     * any other vector that is contained in the square box with sides 2*delta and this
     * vector at the center is considered equal.
     *
     * @param other The other vector to compare this one to
     * @param delta The allowed error
     * @return True iff this vector is equal to other, with a tolerance defined by delta
     */
    public boolean equalsDelta(ROVector2f other, double delta) {
        return (other.getX() - delta < x &&
                other.getX() + delta > x &&
                other.getY() - delta < y &&
                other.getY() + delta > y);

    }

    public DVec toDVec() {
        return new DVec(getX(), getY());
    }
}
