package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * The description of any geometry collision resolver.
 *
 * @author Kevin Glass
 */
public interface Collider {
    /**
     * Determine is any collisions have occured between the two bodies
     * specified.
     *
     * @param contacts The contacts array to populate with results
     * @param bodyA The first body to check against
     * @param bodyB The second body to check against
     * @return The number of contacts that have been determined and hence
     * populated in the array.
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB);
}
