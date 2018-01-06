package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A simple shape describing the area covered by a body
 *
 * @author Kevin Glass
 */
public interface Shape {

    /**
     * Get the box bounds of the shape
     *
     * @return The bounds of the shape
     */
    public AABox getBounds();

    /**
     * Some factor based on the edges length of the shape
     *
     * @return The factor result - from the original demo
     */
    public double getSurfaceFactor();
}
