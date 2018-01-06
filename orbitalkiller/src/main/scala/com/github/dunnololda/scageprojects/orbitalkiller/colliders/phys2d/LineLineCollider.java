package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

/**
 * Collides two lines with oneanother.
 *
 * @author Gideon Smeding
 */
public class LineLineCollider implements Collider {

    /**
     * @see net.phys2d.raw.collide.Collider#collide(net.phys2d.raw.Contact[], net.phys2d.raw.Body, net.phys2d.raw.Body)
     */
    public int collide(Contact[] contacts, Body bodyA, Body bodyB) {
        // TODO: function disabled until we can remember on what side of A,
        // B used to be, which is crucial to determine a proper collision normal
        return 0;
//		Line lineA = (Line) bodyA.getShape();
//		Line lineB = (Line) bodyB.getShape();
//		
//		Vector2f[] vertsA = lineA.getVertices(bodyA.getPosition(), bodyA.getRotation());
//		Vector2f[] vertsB = lineB.getVertices(bodyB.getPosition(), bodyB.getRotation());
//		
//		Vector2f startA = vertsA[0];
//		Vector2f endA = vertsA[1];
//		Vector2f startB = vertsB[0];
//		Vector2f endB = vertsB[1];
//		
//		//TODO: reuse mathutil.intersect?
//		double d = (endB.y - startB.y) * (endA.x - startA.x) - (endB.x - startB.x) * (endA.y - startA.y);
//		
//		if ( d == 0 ) // parallel lines
//			return 0;
//		
//		double uA = (endB.x - startB.x) * (startA.y - startB.y) - (endB.y - startB.y) * (startA.x - startB.x);
//		uA /= d;
//		double uB = (endA.x - startA.x) * (startA.y - startB.y) - (endA.y - startA.y) * (startA.x - startB.x);
//		uB /= d;
//		
//		if ( uA < 0 || uA > 1 || uB < 0 || uB > 1 ) 
//			return 0; // intersection point isn't between the start and endpoints
//		
//		// there must be a collision, let's determine our contact information
//		// we're searching for a contact with the smallest penetration depth
//		Vector2f[][] closestPoints = {
//				{startB, getClosestPoint(startA, endA, startB)},
//				{endB, getClosestPoint(startA, endA, endB)},
//				{startA, getClosestPoint(startB, endB, startA)},
//				{endA, getClosestPoint(startB, endB, endA)}
//		};
//		
//		double distSquared = double.MAX_VALUE;
//		Vector2f position = null;
//		Vector2f normal = new Vector2f();
//		
//		for ( int i = 0; i < 4; i++ ) {
//			Vector2f l;
//			if ( i < 2 ) {
//				l = closestPoints[i][1];
//				l.sub(closestPoints[i][0]);
//			} else {
//				l = closestPoints[i][0];
//				l.sub(closestPoints[i][1]);
//			}
//			
//			double newDistSquared = l.lengthSquared();
//			if ( newDistSquared < distSquared ) {
//				distSquared = newDistSquared;
//				position = closestPoints[i][0];
//				normal.set(l);
//			}
//		}
//		
//		normal.normalise();
//		contacts[0].setNormal(normal);
//		contacts[0].setPosition(position);
//		if ( Math.sqrt(distSquared) > 10f )
//			System.out.println(Math.sqrt(distSquared));
//		contacts[0].setSeparation((double) -Math.sqrt(distSquared));
//		
//		return 1;
    }

    /**
     * Gets the closest point to a given point on the indefinately extended line.
     * TODO: move this somewhere in math package
     *
     * @param startA Starting point of the line
     * @param endA   End point of the line
     * @param point  The point to get a closes point on the line for
     * @return the closest point on the line or null if the lines are parallel
     */
    public static Vector2f getClosestPoint(Vector2f startA, Vector2f endA, Vector2f point) {
        Vector2f startB = point;
        Vector2f endB = new Vector2f(endA);
        endB.sub(startA);
        endB.set(endB.y, -endB.x);

        double d = endB.y * (endA.x - startA.x);
        d -= endB.x * (endA.y - startA.y);

        if (d == 0)
            return null;

        double uA = endB.x * (startA.y - startB.getY());
        uA -= endB.y * (startA.x - startB.getX());
        uA /= d;

        return new Vector2f(
                startA.x + uA * (endA.x - startA.x),
                startA.y + uA * (endA.y - startA.y));
    }

}
