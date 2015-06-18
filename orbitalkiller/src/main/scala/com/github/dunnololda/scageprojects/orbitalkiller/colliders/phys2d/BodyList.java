package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

import java.util.ArrayList;

/**
 * A typed list of <code>Body</code>
 *
 * @author Kevin Glass
 */
public class BodyList {
    /** The elements in the list */
    private ArrayList elements = new ArrayList();

    /**
     * Create an empty list
     */
    public BodyList() {

    }

    /**
     * Create a new list containing the elements specified
     *
     * @param list The list of elements to add to the new list
     */
    BodyList(BodyList list) {
        elements.addAll(list.elements);
    }

    /**
     * Add a body to the list
     *
     * @param body The body to add
     */
    public void add(Body body) {
        elements.add(body);
    }

    /**
     * Get the number of elements in the list
     *
     * @return The number of the element in the list
     */
    public int size() {
        return elements.size();
    }

    /**
     * Remove a body from the list
     *
     * @param body The body to remove from the list 
     */
    public void remove(Body body) {
        elements.remove(body);
    }

    /**
     * Get a body at a specific index
     *
     * @param i The index of the body to retrieve
     * @return The body retrieved
     */
    public Body get(int i) {
        return (Body) elements.get(i);
    }

    /**
     * Clear all the elements out of the list
     */
    public void clear() {
        elements.clear();
    }

    /**
     * Check if this list contains the specified body
     *
     * @param body The body to look for
     * @return True if this list contains the specified body
     */
    public boolean contains(Body body) {
        return elements.contains(body);
    }

    /**
     * Get a list of bodies containing all of the bodies in this
     * list except those specified
     *
     * @param others The bodies that should be removed from the contents
     * @return The list of bodies excluding those specified
     */
    public BodyList getContentsExcluding(BodyList others) {
        BodyList list = new BodyList(this);
        list.elements.removeAll(others.elements);

        return list;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        String str = "[BodyList ";
        for (int i=0;i<elements.size();i++) {
            str += get(i)+",";
        }
        str += "]";

        return str;
    }
}
