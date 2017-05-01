//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Luis Barrancos, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#ifndef AS_MATH_COMPLEX_H
#define AS_MATH_COMPLEX_H

struct Complex
{
    float real;
    float imag;
};

//
// Reference:
//
//      Complex Number Arithmetic
//      https://en.wikipedia.org/wiki/Complex_number
//

void add_complex(Complex augend, Complex addend, output Complex sum)
{
    sum.real = augend.real + addend.real;
    sum.imag = augend.imag + addend.imag;
}

void add_complex(float augend, Complex addend, output Complex sum)
{
    sum.real = augend + addend.real;
    sum.imag = addend.imag;
}

void add_complex(Complex augend, float addend, output Complex sum)
{
    sum.real = augend.real + addend;
    sum.imag = augend.imag;
}

void subtract_complex(
    Complex minuend,
    Complex subtrahend,
    output Complex difference)
{
    difference.real = minuend.real - subtrahend.real;
    difference.imag = minuend.imag - subtrahend.imag;
}

void subtract_complex(
    float minuend,
    Complex subtrahend,
    output Complex difference)
{
    difference.real = minuend - subtrahend.real;
    difference.imag = subtrahend.imag;
}

void subtract_complex(
    Complex minuend,
    float subtrahend,
    output Complex difference)
{
    difference.real = minuend.real - subtrahend;
    difference.imag = minuend.imag;
}

void multiply_complex(
    Complex multiplier,
    Complex multiplicand,
    output Complex product)
{
    product.real = multiplier.real * multiplicand.real -
                   multiplier.imag * multiplicand.imag;

    product.imag = multiplier.real * multiplicand.imag +
                   multiplier.imag * multiplicand.real;
}

void multiply_complex(
    float multiplier,
    Complex multiplicand,
    output Complex product)
{
    product.real = multiplier * multiplicand.real;
    product.imag = multiplier * multiplicand.imag;
}

void multiply_complex(
    Complex multiplier,
    float multiplicand,
    output Complex product)
{
    product.real = multiplier.real * multiplicand;
    product.imag = multiplier.imag * multiplicand;
}

void divide_complex(
    Complex numerator,
    Complex denominator,
    output Complex quotient)
{
    float denom = sqr(denominator.real) + sqr(denominator.imag);

    quotient.real = (numerator.real * denominator.real +
                     numerator.imag * denominator.imag) / denom;
                     
    quotient.imag = (numerator.imag * denominator.real -
                     numerator.real * denominator.imag) / denom;
}

float abs_complex(Complex Z)
{
    return hypot(Z.real, Z.imag);
}

void inverse_complex(output Complex Z)
{
    float denom = sqr(Z.real) + sqr(Z.imag);
    
    Z.real = Z.real / denom;
    Z.imag = -Z.imag / denom;
}

void square_complex(output Complex Z)
{
    float tmp = Z.real;

    Z.real = sqr(Z.real) - sqr(Z.imag);
    Z.imag *= 2 * tmp;
}

void neg_complex(output Complex Z)
{
    Z.real = -Z.real;
    Z.imag = -Z.imag;
}

float arg_complex(Complex Z)
{
    return atan2(Z.imag, Z.real);
}

void pow_complex(
    Complex base,
    Complex exponent,
    output Complex power)
{
    Complex Z = {log(hypot(base.real, base.imag)), arg_complex(base)};

    Complex C = {Z.real * exponent.real - Z.imag * exponent.imag,
                 Z.real * exponent.imag + Z.imag * exponent.real};

    float exp_real = exp(C.real), sintheta, costheta;

    sincos(C.imag, sintheta, costheta);

    power.real = exp_real * costheta;
    power.imag = exp_real * sintheta;
}

void log_complex(Complex Z)
{
    float tmp = Z.imag;

    Z.imag = arg_complex(Z);
    Z.real = hypot(Z.real, tmp);
}

void conjugate_complex(output Complex Z)
{
    Z.imag = -Z.imag;
}

void exp_complex(output Complex Z)
{
    float sin_im_z, cos_im_z, exp_re_z = exp(Z.real);

    sincos(Z.imag, sin_im_z, cos_im_z);

    Z.real = exp_re_z * cos_im_z;
    Z.imag = exp_re_z * sin_im_z;
}

void sqrt_complex(output Complex Z)
{
    if (Z.imag == 0.0 && Z.real != 0.0)
    {
        Z.real = sqrt(Z.real);
    }
    else
    {
        float magnitude = hypot(Z.real, Z.imag);

        if (magnitude > 0.0)
        {
            Z.imag = (Z.imag < 0.0)
                ? -M_SQRT1_2 * sqrt(magnitude - Z.real)
                :  M_SQRT1_2 * sqrt(magnitude - Z.real);

            Z.real = M_SQRT1_2 * sqrt(magnitude + Z.real);
        }
        else
        {
            Z.imag = Z.real = 0.0;
        }
    }
}

#endif // !AS_MATH_COMPLEX_H
