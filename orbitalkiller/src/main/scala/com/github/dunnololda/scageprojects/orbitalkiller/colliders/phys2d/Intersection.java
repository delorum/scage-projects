package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Class representing a single intersection.
 * TODO: this should be merged with the 'Feature' class, it represents the same thing
 *
 * @author Gideon Smeding
 */
public class Intersection {
    /**
     * The edge of polygon A that intersects
     */
    public int edgeA;
    /**
     * The edge of polygon B that intersects
     */
    public int edgeB;

    /**
     * The position of the intersection in world (absolute) coordinates
     */
    public Vector2f position;

    /**
     * True iff this is an intersection where polygon A enters B
     */
    public boolean isIngoing;

    /**
     * Construct an intersection with all its attributes set.
     *
     * @param edgeA     The edge of polygon A that intersects
     * @param edgeB     The edge of polygon B that intersects
     * @param position  The position of the intersection in world (absolute) coordinates
     * @param isIngoing True iff this is an intersection where polygon A enters B
     */
    public Intersection(int edgeA, int edgeB, Vector2f position, boolean isIngoing) {
        super();
        this.edgeA = edgeA;
        this.edgeB = edgeB;
        this.position = position;
        this.isIngoing = isIngoing;
    }
}
