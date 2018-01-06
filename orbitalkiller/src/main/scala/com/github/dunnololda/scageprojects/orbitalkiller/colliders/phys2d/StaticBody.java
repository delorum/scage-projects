package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A body that will not move
 *
 * @author Kevin Glass
 */
public strictfp class StaticBody extends Body {

    /**
     * Create a static body
     *
     * @param shape The shape representing this body
     */
    public StaticBody(Shape shape) {
        super(shape, Body.INFINITE_MASS);
    }

    /**
     * Create a static body
     *
     * @param name  The name to assign to the body
     * @param shape The shape representing this body
     */
    public StaticBody(String name, Shape shape) {
        super(name, shape, Body.INFINITE_MASS);
    }

    /**
     * @see net.phys2d.raw.Body#isRotatable()
     */
    public boolean isRotatable() {
        return false;
    }

    /**
     * @see net.phys2d.raw.Body#isMoveable()
     */
    public boolean isMoveable() {
        return false;
    }

    /**
     * Check if this body is static
     *
     * @return True if this body is static
     */
    public boolean isStatic() {
        return true;
    }

    /**
     * @see net.phys2d.raw.Body#isResting()
     */
    public boolean isResting() {
        return true;
    }
}
