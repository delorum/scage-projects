package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

import com.github.dunnololda.scage.support.DVec;

/**
 * A readonly two dimensional vector
 *
 * @author Kevin Glass
 */
public interface ROVector2f {
    /**
     * Get the X component of this vector
     *
     * @return The X component of this vector
     */
    public double getX();

    /**
     * Get the Y component of this vector
     *
     * @return The Y component of this vector
     */
    public double getY();

    /**
     * Get the length of this vector
     *
     * @return The length of this vector
     */
    public double length();

    /**
     * Get the dot product of this vector and another
     *
     * @param other The other vector to dot against
     * @return The dot product of the two vectors
     */
    public double dot(ROVector2f other);

    /**
     * Project this vector onto another
     *
     * @param b The vector to project onto
     * @param result The projected vector
     */
    public void projectOntoUnit(ROVector2f b, Vector2f result);

    /**
     * The length of the vector squared
     *
     * @return The length of the vector squared
     */
    public double lengthSquared();

    /**
     * Get the distance from this point to another
     *
     * @param other The other point we're measuring to
     * @return The distance to the other point
     */
    public double distance(ROVector2f other);

    /**
     * Get the distance squared from this point to another
     *
     * @param other The other point we're measuring to
     * @return The distance to the other point
     */
    public double distanceSquared(ROVector2f other);

    public DVec toDVec();
}
