package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * A description of a single contact point between two bodies
 *
 * @author Kevin Glass
 */
public strictfp class Contact {
    // TODO: the positions are absolute, right? if not make them so
    /**
     * The position of the contact
     */
    Vector2f position = new Vector2f();
    /**
     * The normal at the contact point which, for convex bodies,
     * points away from the first body.
     */
    Vector2f normal = new Vector2f();
    /**
     * ?
     */
    double separation;
    /**
     * The impulse accumlated in the direction of the normal
     */
    double accumulatedNormalImpulse;
    /**
     * The impulse accumlated in the direction of the tangent
     */
    double accumulatedTangentImpulse;
    /**
     * The mass applied throught the normal at this contact point
     */
    double massNormal;
    /**
     * The mass applied through the tangent at this contact point
     */
    double massTangent;
    /**
     * The correction factor penetration
     */
    double bias;
    /**
     * The pair of edges this contact is between
     */
    FeaturePair feature = new FeaturePair();
    /**
     * The restitution at this point of contact
     */
    double restitution;
    /**
     * The bias impulse accumulated
     */
    double biasImpulse;

    /**
     * Create a new contact point
     */
    public Contact() {
        accumulatedNormalImpulse = 0.0f;
        accumulatedTangentImpulse = 0.0f;
    }

    /**
     * Get the position of this contact
     *
     * @return The position of this contact
     */
    public ROVector2f getPosition() {
        return position;
    }

    /**
     * Set the contact information based on another contact
     *
     * @param contact The contact information
     */
    void set(Contact contact) {
        position.set(contact.position);
        normal.set(contact.normal);
        separation = contact.separation;
        accumulatedNormalImpulse = contact.accumulatedNormalImpulse;
        accumulatedTangentImpulse = contact.accumulatedTangentImpulse;
        massNormal = contact.massNormal;
        massTangent = contact.massTangent;
        bias = contact.bias;
        restitution = contact.restitution;
        feature.set(contact.feature);
    }

    /**
     * Get the seperation between bodies
     *
     * @return The seperation between bodies
     */
    public double getSeparation() {
        return separation;
    }

    /**
     * Get the normal at the point of contact
     *
     * @return The normal at the point of contact
     */
    public ROVector2f getNormal() {
        return normal;
    }

    /**
     * Set the normal at the point of contact.
     *
     * @param normal The normal at the point of contact
     */
    public void setNormal(ROVector2f normal) {
        this.normal.set(normal);
    }

    /**
     * Set the position of the contact
     *
     * @param position The position of the contact
     */
    public void setPosition(ROVector2f position) {
        this.position.set(position);
    }

    /**
     * Get the pairing identifing the location of the contact
     *
     * @return The feature painting identifing the contact
     */
    public FeaturePair getFeature() {
        return feature;
    }

    /**
     * Set the feature identifying the location of the contact
     *
     * @param pair The pair identifying the location of the contact
     */
    public void setFeature(FeaturePair pair) {
        this.feature = pair;
    }

    /**
     * Set the separation between bodies
     *
     * @param separation The separation between bodies at this contact
     */
    public void setSeparation(double separation) {
        this.separation = separation;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return feature.hashCode();
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object other) {
        if (other.getClass() == getClass()) {
            return ((Contact) other).feature.equals(feature);
        }

        return false;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return "[Contact " + position + " n: " + normal + " sep: " + separation + "]";
    }
}
