package com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d;

public strictfp class Matrix2f {
    /** The first column of the matrix */
    public Vector2f col1 = new Vector2f();
    /** The second column of the matrix */
    public Vector2f col2 = new Vector2f();

    /**
     * Create an empty matrix
     */
    public Matrix2f() {
    }

    /**
     * Create a matrix with a rotation
     *
     * @param angle The angle of the rotation decribed by the matrix
     */
    public Matrix2f(double angle)
    {
        double c = Math.cos(angle);
        double s = Math.sin(angle);
        col1.x = c; col2.x = -s;
        col1.y = s; col2.y = c;
    }

    /**
     * Create a matrix
     *
     * @param col1 The first column
     * @param col2 The second column
     */
    public Matrix2f(Vector2f col1, Vector2f col2) {
        this.col1.set(col1);
        this.col2.set(col2);
    }

    /**
     * Transpose the matrix
     *
     * @return A newly created matrix containing the transpose of this matrix
     */
    public Matrix2f transpose()
    {
        return new Matrix2f(new Vector2f(col1.x, col2.x),
                new Vector2f(col1.y, col2.y));
    }

    /**
     * Transpose the invert
     *
     * @return A newly created matrix containing the invert of this matrix
     */
    public Matrix2f invert()
    {
        double a = col1.x, b = col2.x, c = col1.y, d = col2.y;
        Matrix2f B = new Matrix2f();

        double det = a * d - b * c;
        if (det == 0.0f) {
            throw new RuntimeException("Matrix2f: invert() - determinate is zero!");
        }

        det = 1.0f / det;
        B.col1.x =  det * d;	B.col2.x = -det * b;
        B.col1.y = -det * c;	B.col2.y =  det * a;
        return B;
    }
}
