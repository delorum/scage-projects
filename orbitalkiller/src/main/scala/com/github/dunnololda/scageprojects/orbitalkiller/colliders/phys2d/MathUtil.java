package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

public final strictfp class MathUtil {
    /** Prevent construction */
    private MathUtil() {}

    /**
     * Scale a vector by a given value
     *
     * @param a The vector to be scaled
     * @param scale The amount to scale the vector by
     * @return A newly created vector - a scaled version of the new vector
     */
    public static Vector2f scale(ROVector2f a, double scale) {
        Vector2f temp = new Vector2f(a);
        temp.scale(scale);

        return temp;
    }

    /**
     * Subtract one vector from another
     *
     * @param a The vector to be subtracted from
     * @param b The vector to subtract
     * @return A newly created containing the result
     */
    public static Vector2f sub(ROVector2f a, ROVector2f b) {
        Vector2f temp = new Vector2f(a);
        temp.sub(b);

        return temp;
    }

    /**
     * Check the sign of a value 
     *
     * @param x The value to check
     * @return -1.0f if negative, 1.0 if positive
     */
    public static double sign(double x)
    {
        return x < 0.0f ? -1.0f : 1.0f;
    }

    /**
     * Multiply a matrix by a vector
     *
     * @param A The matrix to be multiplied
     * @param v The vector to multiple by
     * @return A newly created vector containing the resultant vector
     */
    public static Vector2f mul(Matrix2f A, ROVector2f v)
    {
        return new Vector2f(A.col1.x * v.getX() + A.col2.x * v.getY(), A.col1.y * v.getX() + A.col2.y * v.getY());
    }

    /**
     * Multiple two matricies
     *
     * @param A The first matrix
     * @param B The second matrix
     * @return A newly created matrix containing the result
     */
    public static Matrix2f mul(Matrix2f A, Matrix2f B)
    {
        return new Matrix2f(mul(A, B.col1), mul(A, B.col2));
    }

    /**
     * Create the absolute version of a matrix
     *
     * @param A The matrix to make absolute
     * @return A newly created absolute matrix
     */
    public static Matrix2f abs(Matrix2f A) {
        return new Matrix2f(abs(A.col1), abs(A.col2));
    }

    /**
     * Make a vector absolute
     *
     * @param a The vector to make absolute
     * @return A newly created result vector
     */
    public static Vector2f abs(Vector2f a)
    {
        return new Vector2f(Math.abs(a.x), Math.abs(a.y));
    }

    /**
     * Add two matricies
     *
     * @param A The first matrix
     * @param B The second matrix
     * @return A newly created matrix containing the result
     */
    public static Matrix2f add(Matrix2f A, Matrix2f B)
    {
        Vector2f temp1 = new Vector2f(A.col1);
        temp1.add(B.col1);
        Vector2f temp2 = new Vector2f(A.col2);
        temp2.add(B.col2);

        return new Matrix2f(temp1,temp2);
    }

    /**
     * Find the cross product of two vectors
     *
     * @param a The first vector
     * @param b The second vector
     * @return The cross product of the two vectors
     */
    public static double cross(Vector2f a, Vector2f b)
    {
        return a.x * b.y - a.y * b.x;
    }

    /**
     * Find the cross product of a vector and a double
     *
     * @param s The scalar double
     * @param a The vector to fidn the cross of
     * @return A newly created resultant vector
     */
    public static Vector2f cross(double s, Vector2f a)
    {
        return new Vector2f(-s * a.y, s * a.x);
    }

    /**
     * Find the cross product of a vector and a double
     *
     * @param s The scalar double
     * @param a The vector to fidn the cross of
     * @return A newly created resultant vector
     */
    public static Vector2f cross(Vector2f a, double s)
    {
        return new Vector2f(s * a.y, -s * a.x);
    }

    /**
     * Clamp a value 
     *
     * @param a The original value
     * @param low The lower bound
     * @param high The upper bound
     * @return The clamped value
     */
    public static double clamp(double a, double low, double high)
    {
        return Math.max(low, Math.min(a, high));
    }


    /**
     * Get the normal of a line x y (or edge). 
     * When standing on x facing y, the normal will point
     * to the left.
     *
     * TODO: move this function somewhere else?
     *
     * @param x startingpoint of the line
     * @param y endpoint of the line
     * @return a (normalised) normal
     */
    public static Vector2f getNormal(ROVector2f x, ROVector2f y) {
        Vector2f normal = new Vector2f(y);
        normal.sub(x);

        normal = new Vector2f(normal.y, -normal.x);
        normal.normalise();

        return normal;
    }

//	public static Vector2f intersect(Vector2f startA, Vector2f endA, Vector2f startB, Vector2f endB) {				
//		double d = (endB.y - startB.y) * (endA.x - startA.x) - (endB.x - startB.x) * (endA.y - startA.y);
//		
//		if ( d == 0 ) // parallel lines
//			return null;
//		
//		double uA = (endB.x - startB.x) * (startA.y - startB.y) - (endB.y - startB.y) * (startA.x - startB.x);
//		uA /= d;
//		double uB = (endA.x - startA.x) * (startA.y - startB.y) - (endA.y - startA.y) * (startA.x - startB.x);
//		uB /= d;
//		
//		if ( uA < 0 || uA > 1 || uB < 0 || uB > 1 ) 
//			return null; // intersection point isn't between the start and endpoints
//		
//		return new Vector2f(
//				startA.x + uA * (endA.x - startA.x),
//				startA.y + uA * (endA.y - startA.y));
//	}


}
