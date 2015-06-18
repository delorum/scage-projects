package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * An axis oriented used for shape bounds
 *
 * @author Kevin Glass
 */
public strictfp class AABox {
    /** The width of the box */
    private double width;
    /** The height of the box */
    private double height;
    /** The x offset to the body's position of the bounds */
    private double offsetx;
    /** The y offset to the body's position of the bounds */
    private double offsety;

    /**
     * Create a new bounding box
     *
     * @param width The width of the box
     * @param height The hieght of the box
     */
    public AABox(double width, double height) {
        this.width = width;
        this.height = height;
    }

    /**
     * Create a new AABox
     *
     * @param offsetx The x offset to the body's position
     * @param offsety The y offset to the body's position
     * @param width The width of the box
     * @param height The hieght of the box
     */
    public AABox(double offsetx, double offsety, double width, double height) {
        this.width = width;
        this.height = height;
        this.offsetx = offsetx;
        this.offsety = offsety;
    }

    /**
     * Get the width of the box
     *
     * @return The width of the box
     */
    public double getWidth() {
        return width;
    }

    /**
     * Get the height of the box
     *
     * @return The height of the box
     */
    public double getHeight() {
        return height;
    }

    /**
     * Get the x offset to the body's position of this bounds
     *
     * @return The x offset to the body's position of this bounds
     */
    public double getOffsetX() {
        return offsetx;
    }

    /**
     * Get the y offset to the body's position of this bounds
     *
     * @return The y offset to this body's position of this bounds
     */
    public double getOffsetY() {
        return offsety;
    }

    /**
     * Check if this box touches another
     *
     * @param x The x position of this box
     * @param y The y position of this box
     * @param other The other box to check against  
     * @param otherx The other box's x position
     * @param othery The other box's y position
     * @return True if the boxes touches
     */
    public boolean touches(double x, double y, AABox other, double otherx, double othery) {
        double totalWidth = (other.width + width) / 2;
        double totalHeight = (other.height + height) / 2;

        double dx = Math.abs((x + offsetx) - (otherx + other.offsetx));
        double dy = Math.abs((y + offsety) - (othery + other.offsety));

        return (totalWidth > dx) && (totalHeight > dy);
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return "[AABox "+width+"x"+height+"]";
    }
}
