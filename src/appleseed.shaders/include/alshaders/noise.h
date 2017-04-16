
/* srdnoise23, Simplex noise with rotating gradients
 * and a true analytic derivative in 2D and 3D.
 *
 * This is version 2 of srdnoise23 written in early 2008.
 * A stupid bug was corrected. Do not use earlier versions.
 *
 * Author: Stefan Gustavson, 2003-2008
 *
 * Contact: stefan.gustavson@gmail.com
 */

 /*
This code was GPL licensed until February 2011.
As the original author of this code, I hereby
release it irrevocably into the public domain.
Please feel free to use it for whatever you want.
Credit is appreciated where appropriate, and I also
appreciate being told where this code finds any use,
but you may do as you like. Alternatively, if you want
to have a familiar OSI-approved license, you may use
This code under the terms of the MIT license:

Copyright (C) 2003-2005 by Stefan Gustavson. All rights reserved.
This code is licensed to you under the terms of the MIT license:

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#ifndef ALSHADERS_OSL_NOISE_H
#define ALSHADERS_OSL_NOISE_H

/** \file
    \brief C implementation of Perlin simplex noise with rotating
    gradients and analytic derivative over 2 and 3 dimensions.
    \author Stefan Gustavson (stefan.gustavson@gmail.com)
*/

/*
 * This is an implementation of Perlin "simplex noise" over two dimensions
 * (x,y) and three dimensions (x,y,z). One extra parameter 't' rotates the
 * underlying gradients of the grid, which gives a swirling, flow-like
 * motion. The derivative is returned, to make it possible to do pseudo-
 * advection and implement "flow noise", as presented by Ken Perlin and
 * Fabrice Neyret at Siggraph 2001.
 *
 * When not animated and presented in one octave only, this noise
 * looks exactly the same as the plain version of simplex noise.
 * It's nothing magical by itself, although the extra animation
 * parameter 't' is useful. Fun stuff starts to happen when you
 * do fractal sums of several octaves, with different rotation speeds
 * and an advection of smaller scales by larger scales (or even the
 * other way around it you feel adventurous).
 *
 * The gradient rotations that can be performed by this noise function
 * and the true analytic derivatives are required to do flow noise.
 * You can't do it properly with regular Perlin noise.
 *
 */

/* a = sqrt(2)/sqrt(3) = 0.816496580 */
#define a 0.81649658

/* Skewing factors for 3D simplex grid:
* F3 = 1/3
* G3 = 1/6 */
#define F3 0.333333333
#define G3 0.166666667

float srdnoise3(
    float           x,
    float           y,
    float           z,
    float           angle,
    output float    dnoise_dx,
    output float    dnoise_dy,
    output float    dnoise_dz)
{
    /*
    * Gradient tables. These could be programmed the Ken Perlin way with
    * some clever bit-twiddling, but this is more clear, and not really slower.
    *
    * For 3D, we define two orthogonal vectors in the desired rotation plane.
    * These vectors are based on the midpoints of the 12 edges of a cube,
    * they all rotate in their own plane and are never coincident or collinear.
    * A larger array of random vectors would also do the job, but these 12
    * (including 4 repeats to make the array length a power of two) work better.
    * They are not random, they are carefully chosen to represent a small
    * isotropic set of directions for any rotation angle.
    */
    float grad3u[48] =
    {
        1.0, 0.0, 1.0, 0.0, 1.0, 1.0, // 12 cube edges
        -1.0, 0.0, 1.0, 0.0, -1.0, 1.0,
        1.0, 0.0, -1.0, 0.0, 1.0, -1.0,
        -1.0, 0.0, -1.0, 0.0, -1.0, -1.0,
        a, a, a, -a, a, -a,
        -a, -a, a, a, -a, -a,
        -a, a, a, a, -a, a,
        a, -a, -a, -a, a, -a
    };

    float grad3v[48] =
    {
        -a, a, a, -a, -a, a,
        a, -a, a, a, a, a,
        -a, -a, -a, a, -a, -a,
        a, a, -a, -a, a, -a,
        1.0, -1.0, 0.0, 1.0, 1.0, 0.0,
        -1.0, 1.0, 0.0, -1.0, -1.0, 0.0,
        1.0, 0.0, 1.0, -1.0, 0.0, 1.0, // 4 repeats to make 16
        0.0, 1.0, -1.0, 0.0, -1.0, -1.0
    };

    int perm[512] =
    {
        110, 43, 157, 38, 11, 85, 117, 192, 115, 149, 77, 86, 185, 31, 129, 95, 1, 147, 217, 173, 107, 196, 213, 241, 106, 10, 178, 203, 204, 15, 199, 252, 0, 37, 183, 205, 131, 29, 119, 103, 187, 76, 170, 133, 40, 74, 197, 231, 186, 87, 39, 47, 46, 25, 150, 232, 240, 249, 81, 206, 24, 175, 201, 101, 135, 112, 243, 79, 198, 35, 126, 68, 114, 53, 59, 78, 242, 177, 202, 144, 16, 14, 219, 211, 100, 143, 80, 60, 58, 208, 105, 26, 124, 6, 9, 23, 237, 8, 155, 180, 190, 250, 94, 122, 174, 57, 220, 70, 45, 30, 28, 121, 49, 195, 4, 248, 66, 17, 90, 113, 154, 134, 215, 153, 216, 88, 168, 127, 230, 137, 228, 254, 71, 246, 7, 156, 63, 98, 182, 224, 128, 64, 21, 251, 167, 200, 161, 91, 244, 152, 2, 104, 12, 34, 96, 54, 111, 226, 20, 108, 239, 247, 33, 44, 120, 42, 179, 52, 172, 207, 235, 83, 18, 146, 191, 222, 125, 48, 92, 82, 5, 102, 181, 19, 56, 233, 176, 61, 238, 109, 89, 132, 118, 223, 141, 163, 193, 229, 142, 234, 227, 55, 51, 194, 171, 93, 245, 145, 188, 130, 189, 27, 255, 69, 140, 97, 67, 169, 214, 253, 184, 164, 138, 3, 212, 75, 139, 218, 225, 165, 236, 166, 148, 36, 84, 62, 32, 160, 72, 221, 210, 209, 136, 73, 13, 50, 162, 22, 158, 116, 123, 41, 159, 151, 65, 99,
        110, 43, 157, 38, 11, 85, 117, 192, 115, 149, 77, 86, 185, 31, 129, 95, 1, 147, 217, 173, 107, 196, 213, 241, 106, 10, 178, 203, 204, 15, 199, 252, 0, 37, 183, 205, 131, 29, 119, 103, 187, 76, 170, 133, 40, 74, 197, 231, 186, 87, 39, 47, 46, 25, 150, 232, 240, 249, 81, 206, 24, 175, 201, 101, 135, 112, 243, 79, 198, 35, 126, 68, 114, 53, 59, 78, 242, 177, 202, 144, 16, 14, 219, 211, 100, 143, 80, 60, 58, 208, 105, 26, 124, 6, 9, 23, 237, 8, 155, 180, 190, 250, 94, 122, 174, 57, 220, 70, 45, 30, 28, 121, 49, 195, 4, 248, 66, 17, 90, 113, 154, 134, 215, 153, 216, 88, 168, 127, 230, 137, 228, 254, 71, 246, 7, 156, 63, 98, 182, 224, 128, 64, 21, 251, 167, 200, 161, 91, 244, 152, 2, 104, 12, 34, 96, 54, 111, 226, 20, 108, 239, 247, 33, 44, 120, 42, 179, 52, 172, 207, 235, 83, 18, 146, 191, 222, 125, 48, 92, 82, 5, 102, 181, 19, 56, 233, 176, 61, 238, 109, 89, 132, 118, 223, 141, 163, 193, 229, 142, 234, 227, 55, 51, 194, 171, 93, 245, 145, 188, 130, 189, 27, 255, 69, 140, 97, 67, 169, 214, 253, 184, 164, 138, 3, 212, 75, 139, 218, 225, 165, 236, 166, 148, 36, 84, 62, 32, 160, 72, 221, 210, 209, 136, 73, 13, 50, 162, 22, 158, 116, 123, 41, 159, 151, 65, 99
    };

    void gradrot3(
        int             hash,
        float           sin_t,
        float           cos_t,
        output float    gx,
        output float    gy,
        output float    gz)
    {
        int h = hash & 15;
        float gux = grad3u[h * 3 + 0];
        float guy = grad3u[h * 3 + 1];
        float guz = grad3u[h * 3 + 2];
        float gvx = grad3v[h * 3 + 0];
        float gvy = grad3v[h * 3 + 1];
        float gvz = grad3v[h * 3 + 2];
        gx = cos_t * gux + sin_t * gvx;
        gy = cos_t * guy + sin_t * gvy;
        gz = cos_t * guz + sin_t * gvz;
    }

    float graddotp3(float gx, float gy, float gz, float x, float y, float z)
    {
        return gx * x + gy * y + gz * z;
    }

    float n0, n1, n2, n3; /* Noise contributions from the four simplex corners */
    float noise;          /* Return value */
    float gx0, gy0, gz0, gx1, gy1, gz1; /* Gradients at simplex corners */
    float gx2, gy2, gz2, gx3, gy3, gz3;
    float sin_t, cos_t; /* Sine and cosine for the gradient rotation angle */
    sin_t = sin(angle);
    cos_t = cos(angle);

    /* Skew the input space to determine which simplex cell we're in */
    float s = (x + y + z) * F3; /* Very nice and simple skew factor for 3D */
    float xs = x + s;
    float ys = y + s;
    float zs = z + s;
    int i = int(floor(xs));
    int j = int(floor(ys));
    int k = int(floor(zs));

    float t = (i + j + k) * G3;
    float X0 = i - t; /* Unskew the cell origin back to (x,y,z) space */
    float Y0 = j - t;
    float Z0 = k - t;
    float x0 = x - X0; /* The x,y,z distances from the cell origin */
    float y0 = y - Y0;
    float z0 = z - Z0;

    /* For the 3D case, the simplex shape is a slightly irregular tetrahedron.
    * Determine which simplex we are in. */
    int i1, j1, k1; /* Offsets for second corner of simplex in (i,j,k) coords */
    int i2, j2, k2; /* Offsets for third corner of simplex in (i,j,k) coords */

    /* TODO: This code would benefit from a backport from the GLSL version! */
    if(x0 >= y0)
    {
        if(y0 >= z0)
        {
            i1 = 1; j1 = 0; k1 = 0; i2 = 1; j2 = 1; k2 = 0; /* X Y Z order */
        }
        else if(x0 >= z0)
        {
            i1 = 1; j1 = 0; k1 = 0; i2 = 1; j2 = 0; k2 = 1; /* X Z Y order */
        }
        else
        {
            i1 = 0; j1 = 0; k1 = 1; i2 = 1; j2 = 0; k2 = 1; /* Z X Y order */
        }
    }
    else // x0 < y0
    {
        if(y0 < z0)
        {
            i1 = 0; j1 = 0; k1 = 1; i2 = 0; j2 = 1; k2 = 1; /* Z Y X order */
        }
        else if(x0 < z0)
        {
            i1 = 0; j1 = 1; k1 = 0; i2 = 0; j2 = 1; k2 = 1; /* Y Z X order */
        }
        else
        {
            i1 = 0; j1 = 1; k1 = 0; i2 = 1; j2 = 1; k2 = 0; /* Y X Z order */
        }
    }

    /* A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
    * a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
    * a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
    * c = 1/6.   */

    float x1 = x0 - i1 + G3; /* Offsets for second corner in (x,y,z) coords */
    float y1 = y0 - j1 + G3;
    float z1 = z0 - k1 + G3;
    float x2 = x0 - i2 + 2.0 * G3; /* Offsets for third corner in (x,y,z) coords */
    float y2 = y0 - j2 + 2.0 * G3;
    float z2 = z0 - k2 + 2.0 * G3;
    float x3 = x0 - 1.0 + 3.0 * G3; /* Offsets for last corner in (x,y,z) coords */
    float y3 = y0 - 1.0 + 3.0 * G3;
    float z3 = z0 - 1.0 + 3.0 * G3;

    /* Wrap the integer indices at 256, to avoid indexing perm[] out of bounds */
    int ii = i % 256;
    if (ii < 0) ii = 255 + ii;

    int jj = j % 256;
    if (jj < 0) jj = 255 + jj;

    int kk = k % 256;
    if (kk < 0) kk = 255 + kk;

    /* Calculate the contribution from the four corners */
    float t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0;
    float t20, t40;

    if(t0 < 0.0)
        n0 = t0 = t20 = t40 = gx0 = gy0 = gz0 = 0.0;
    else
    {
        int idx = ii + perm[jj + perm[kk]];
        gradrot3(perm[idx], sin_t, cos_t, gx0, gy0, gz0);
        t20 = t0 * t0;
        t40 = t20 * t20;
        n0 = t40 * graddotp3(gx0, gy0, gz0, x0, y0, z0);
    }

    float t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1;
    float t21, t41;
    if(t1 < 0.0)
        n1 = t1 = t21 = t41 = gx1 = gy1 = gz1 = 0.0;
    else
    {
        int idx = ii + i1 + perm[jj + j1 + perm[kk + k1]];
        gradrot3(perm[idx], sin_t, cos_t, gx1, gy1, gz1);
        t21 = t1 * t1;
        t41 = t21 * t21;
        n1 = t41 * graddotp3(gx1, gy1, gz1, x1, y1, z1);
    }

    float t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2;
    float t22, t42;

    if(t2 < 0.0)
        n2 = t2 = t22 = t42 = gx2 = gy2 = gz2 = 0.0;
    else
    {
        int idx = ii + i2 + perm[jj + j2 + perm[kk + k2]];
        gradrot3(perm[idx], sin_t, cos_t, gx2, gy2, gz2);
        t22 = t2 * t2;
        t42 = t22 * t22;
        n2 = t42 * graddotp3(gx2, gy2, gz2, x2, y2, z2);
    }

    float t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3;
    float t23, t43;

    if(t3 < 0.0)
        n3 = t3 = t23 = t43 = gx3 = gy3 = gz3 = 0.0;
    else
    {
        int idx = ii + 1 + perm[jj + 1 + perm[kk + 1]];
        gradrot3(perm[idx], sin_t, cos_t, gx3, gy3, gz3);
        t23 = t3 * t3;
        t43 = t23 * t23;
        n3 = t43 * graddotp3(gx3, gy3, gz3, x3, y3, z3);
    }

    /*  Add contributions from each corner to get the final noise value.
        * The result is scaled to return values in the range [-1,1] */
    noise = 28.0 * (n0 + n1 + n2 + n3);

    /* Compute derivatives */
    /*  A straight, unoptimised calculation would be like:
    *    *dnoise_dx = -8.0 * t20 * t0 * x0 * graddotp3(gx0, gy0, gz0, x0, y0, z0) + t40 * gx0;
    *    *dnoise_dy = -8.0 * t20 * t0 * y0 * graddotp3(gx0, gy0, gz0, x0, y0, z0) + t40 * gy0;
    *    *dnoise_dz = -8.0 * t20 * t0 * z0 * graddotp3(gx0, gy0, gz0, x0, y0, z0) + t40 * gz0;
    *    *dnoise_dx += -8.0 * t21 * t1 * x1 * graddotp3(gx1, gy1, gz1, x1, y1, z1) + t41 * gx1;
    *    *dnoise_dy += -8.0 * t21 * t1 * y1 * graddotp3(gx1, gy1, gz1, x1, y1, z1) + t41 * gy1;
    *    *dnoise_dz += -8.0 * t21 * t1 * z1 * graddotp3(gx1, gy1, gz1, x1, y1, z1) + t41 * gz1;
    *    *dnoise_dx += -8.0 * t22 * t2 * x2 * graddotp3(gx2, gy2, gz2, x2, y2, z2) + t42 * gx2;
    *    *dnoise_dy += -8.0 * t22 * t2 * y2 * graddotp3(gx2, gy2, gz2, x2, y2, z2) + t42 * gy2;
    *    *dnoise_dz += -8.0 * t22 * t2 * z2 * graddotp3(gx2, gy2, gz2, x2, y2, z2) + t42 * gz2;
    *    *dnoise_dx += -8.0 * t23 * t3 * x3 * graddotp3(gx3, gy3, gz3, x3, y3, z3) + t43 * gx3;
    *    *dnoise_dy += -8.0 * t23 * t3 * y3 * graddotp3(gx3, gy3, gz3, x3, y3, z3) + t43 * gy3;
    *    *dnoise_dz += -8.0 * t23 * t3 * z3 * graddotp3(gx3, gy3, gz3, x3, y3, z3) + t43 * gz3;
    */
    float temp0 = t20 * t0 * graddotp3(gx0, gy0, gz0, x0, y0, z0);
    dnoise_dx = temp0 * x0;
    dnoise_dy = temp0 * y0;
    dnoise_dz = temp0 * z0;

    float temp1 = t21 * t1 * graddotp3(gx1, gy1, gz1, x1, y1, z1);
    dnoise_dx += temp1 * x1;
    dnoise_dy += temp1 * y1;
    dnoise_dz += temp1 * z1;

    float temp2 = t22 * t2 * graddotp3(gx2, gy2, gz2, x2, y2, z2);
    dnoise_dx += temp2 * x2;
    dnoise_dy += temp2 * y2;
    dnoise_dz += temp2 * z2;

    float temp3 = t23 * t3 * graddotp3(gx3, gy3, gz3, x3, y3, z3);
    dnoise_dx += temp3 * x3;
    dnoise_dy += temp3 * y3;
    dnoise_dz += temp3 * z3;
    dnoise_dx *= -8.0;
    dnoise_dy *= -8.0;
    dnoise_dz *= -8.0;

    /* This corrects a bug in the original implementation */
    dnoise_dx += t40 * gx0 + t41 * gx1 + t42 * gx2 + t43 * gx3;
    dnoise_dy += t40 * gy0 + t41 * gy1 + t42 * gy2 + t43 * gy3;
    dnoise_dz += t40 * gz0 + t41 * gz1 + t42 * gz2 + t43 * gz3;
    dnoise_dx *= 28.0; /* Scale derivative to match the noise scaling */
    dnoise_dy *= 28.0;
    dnoise_dz *= 28.0;

    return noise;
}

float srdnoise(point p, float t, output vector deriv)
{
    return srdnoise3(p[0], p[1], p[2], t, deriv[0], deriv[1], deriv[2]);
}

#undef F3
#undef G3
#undef a

#endif // ALSHADERS_OSL_NOISE_H
