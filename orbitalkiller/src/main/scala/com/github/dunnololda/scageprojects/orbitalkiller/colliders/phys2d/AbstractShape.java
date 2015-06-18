package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Super class of generic shapes in the engine
 *
 * @author Kevin Glass
 */
public strictfp abstract class AbstractShape implements Shape {
    /** The circular bounds that fit the shape based on the position of the body */
    protected AABox bounds;

    /**
     * Construct a new shape as subclas swhich will specified it's
     * own bounds
     */
    protected AbstractShape() {
    }

    /**
     * Create a shape
     *
     * @param bounds The bounds of the shape
     */
    public AbstractShape(AABox bounds) {
        this.bounds = bounds;
    }

    /**
     * @see Shape#getBounds()
     */
    public AABox getBounds() {
        return bounds;
    }
}
