
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_FP_H
#define APPLESEED_FOUNDATION_MATH_FP_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/typetraits.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// IEEE-754 floating-point utility functions.
//
// The methods of this class manipulate floating-point values
// through their binary representation, using integer arithmetic
// only, and thus are fully deterministic and portable.
//
// References:
//
//   http://en.wikipedia.org/wiki/IEEE_754
//   http://babbage.cs.qc.edu/courses/cs341/IEEE-754.html
//

template <typename T>
struct FP;                                                  // intentionally left unimplemented

template <>
struct FP<float>
{
    // Binary representation of special 32-bit floating-point values.
    static const uint32 PosZero = 0x00000000UL;             // +0.0
    static const uint32 NegZero = 0x80000000UL;             // -0.0
    static const uint32 PosMin  = 0x00800000UL;             // +1.175494351e-38f
    static const uint32 NegMin  = 0x80800000UL;             // -1.175494351e-38f
    static const uint32 PosInf  = 0x7F800000UL;             // +infinity
    static const uint32 NegInf  = 0xFF800000UL;             // -infinity
    static const uint32 SNaN    = 0x7FFFFFFFUL;             // signaling NaN
    static const uint32 QNaN    = 0xFFFFFFFFUL;             // quiet NaN (indefinite)
    static const uint32 Ind     = QNaN;                     // synonym for QNaN

    // Useful bitmasks.
    static const uint32 AbsMask = 0x7FFFFFFFUL;             // absolute value bitmask

    // Return special 32-bit floating-point values.
    static float pos_zero();                                // +0.0
    static float neg_zero();                                // -0.0
    static float pos_min();                                 // +1.175494351e-38f
    static float neg_min();                                 // -1.175494351e-38f
    static float pos_inf();                                 // +infinity
    static float neg_inf();                                 // -infinity
    static float snan();                                    // signaling NaN
    static float qnan();                                    // quiet NaN (indefinite)
    static float indefinite();                              // synonym for qnan()

    // Extract the sign bit from a floating-point number.
    static uint32 sign(float x);

    // Extract the exponent bits from a floating-point number.
    static uint32 exponent(float x);

    // Extract the mantissa bits from a floating-point number.
    static uint32 mantissa(float x);

    // Return true if x is a normalized number.
    static bool is_normal(float x);

    // Return true if x is a subnormal (denormalized) number.
    static bool is_subnormal(float x);

    // Return true if x equals +0.0 or -0.0.
    static bool is_zero(float x);

    // Return true if x equals +0.0.
    static bool is_pos_zero(float x);

    // Return true if x equals -0.0.
    static bool is_neg_zero(float x);

    // Return true if x equals +infinity or -infinity.
    static bool is_inf(float x);

    // Return true if x equals +infinity.
    static bool is_pos_inf(float x);

    // Return true if x equals -infinity.
    static bool is_neg_inf(float x);

    // Return true if x equals signaling NaN or quiet NaN (indefinite).
    static bool is_nan(float x);

    // Return true if x equals signaling NaN.
    static bool is_snan(float x);

    // Return true if x equals quiet NaN.
    static bool is_qnan(float x);

    // Construct a 32-bit floating-point value given a sign, an exponent and a mantissa.
    static float construct(
        uint32 sign,
        uint32 exponent,
        uint32 mantissa);
};

template <>
struct FP<double>
{
    // Binary representation of special 64-bit floating-point values.
    static const uint64 PosZero = 0x0000000000000000LL;     // +0.0
    static const uint64 NegZero = 0x8000000000000000LL;     // -0.0
    static const uint64 PosMin  = 0x0080000000000000LL;     // +1.175494351e-38f
    static const uint64 NegMin  = 0x8080000000000000LL;     // -1.175494351e-38f
    static const uint64 PosInf  = 0x7FF0000000000000LL;     // +infinity
    static const uint64 NegInf  = 0xFFF0000000000000LL;     // -infinity
    static const uint64 SNaN    = 0x7FFFFFFFFFFFFFFFLL;     // signaling NaN
    static const uint64 QNaN    = 0xFFFFFFFFFFFFFFFFLL;     // quiet NaN (indefinite)
    static const uint64 Ind     = QNaN;                     // synonym for QNaN

    // Useful bitmasks.
    static const uint64 AbsMask = 0x7FFFFFFFFFFFFFFFLL;     // absolute value bitmask

    // Return special 64-bit floating-point values.
    static double pos_zero();                               // +0.0
    static double neg_zero();                               // -0.0
    static double pos_min();                                // +1.175494351e-38f
    static double neg_min();                                // -1.175494351e-38f
    static double pos_inf();                                // +infinity
    static double neg_inf();                                // -infinity
    static double snan();                                   // signaling NaN
    static double qnan();                                   // quiet NaN (indefinite)
    static double indefinite();                             // synonym for qnan()

    // Extract the sign bit from a floating-point number.
    static uint64 sign(double x);

    // Extract the exponent bits from a floating-point number.
    static uint64 exponent(double x);

    // Extract the mantissa bits from a floating-point number.
    static uint64 mantissa(double x);

    // Return true if x is a normalized number.
    static bool is_normal(double x);

    // Return true if x is a subnormal (denormalized) number.
    static bool is_subnormal(double x);

    // Return true if x equals +0.0 or -0.0.
    static bool is_zero(double x);

    // Return true if x equals +0.0.
    static bool is_pos_zero(double x);

    // Return true if x equals -0.0.
    static bool is_neg_zero(double x);

    // Return true if x equals +infinity or -infinity.
    static bool is_inf(double x);

    // Return true if x equals +infinity.
    static bool is_pos_inf(double x);

    // Return true if x equals -infinity.
    static bool is_neg_inf(double x);

    // Return true if x equals signaling NaN or quiet NaN (indefinite).
    static bool is_nan(double x);

    // Return true if x equals signaling NaN.
    static bool is_snan(double x);

    // Return true if x equals quiet NaN.
    static bool is_qnan(double x);

    // Construct a 64-bit floating-point value given a sign, an exponent and a mantissa.
    static double construct(
        uint64 sign,
        uint64 exponent,
        uint64 mantissa);
};


//
// Shift a scalar x by a given number n of ulp, toward +infinity
// if n is positive, toward -infinity if n is negative. x is left
// unchanged if n equals zero. x must be a normalized floating-point
// number, or +/-infinity. The return value is always a normalized
// floating-point number, or +infinity in case of overflow,
// or -infinity in case of underflow.
//

template <typename T>
T shift(const T x, const typename TypeConv<T>::Int n);


//
// FP<float> class implementation.
//

inline uint32 FP<float>::sign(float x)
{
    return (binary_cast<uint32>(x) & 0x80000000L) >> 31;
}

inline uint32 FP<float>::exponent(float x)
{
    return (binary_cast<uint32>(x) >> 23) & 255;
}

inline uint32 FP<float>::mantissa(float x)
{
    return binary_cast<uint32>(x) & 0x007FFFFFL;
}

inline bool FP<float>::is_normal(float x)
{
    return exponent(x) > 0 && exponent(x) < 255;
}

// Return true if x is a subnormal (denormalized) number.
inline bool FP<float>::is_subnormal(float x)
{
    return exponent(x) == 0 && mantissa(x) != 0;
}

inline bool FP<float>::is_zero(float x)
{
    return (binary_cast<uint32>(x) & 0x7FFFFFFFL) == 0;
}

inline bool FP<float>::is_pos_zero(float x)
{
    return binary_cast<uint32>(x) == PosZero;
}

inline bool FP<float>::is_neg_zero(float x)
{
    return binary_cast<uint32>(x) == NegZero;
}

inline bool FP<float>::is_inf(float x)
{
    return (binary_cast<uint32>(x) & 0x7FFFFFFFL) == PosInf;
}

inline bool FP<float>::is_pos_inf(float x)
{
    return binary_cast<uint32>(x) == PosInf;
}

inline bool FP<float>::is_neg_inf(float x)
{
    return binary_cast<uint32>(x) == NegInf;
}

inline bool FP<float>::is_nan(float x)
{
    return exponent(x) == 255 && mantissa(x) != 0;
}

inline bool FP<float>::is_snan(float x)
{
    return sign(x) == 0 && is_nan(x);
}

inline bool FP<float>::is_qnan(float x)
{
    return sign(x) == 1 && is_nan(x);
}

inline float FP<float>::construct(
    uint32 sign,
    uint32 exponent,
    uint32 mantissa)
{
    assert(sign < 2);
    assert(exponent < (uint32(1) << 8));
    assert(mantissa < (uint32(1) << 23));

    const uint32 value =
          (sign     << 31)
        | (exponent << 22)
        |  mantissa;

    return binary_cast<float>(value);
}

inline float FP<float>::pos_zero()
{
    return binary_cast<float>(PosZero);
}

inline float FP<float>::neg_zero()
{
    return binary_cast<float>(NegZero);
}

inline float FP<float>::pos_min()
{
    return binary_cast<float>(PosMin);
}

inline float FP<float>::neg_min()
{
    return binary_cast<float>(NegMin);
}

inline float FP<float>::pos_inf()
{
    return binary_cast<float>(PosInf);
}

inline float FP<float>::neg_inf()
{
    return binary_cast<float>(NegInf);
}

inline float FP<float>::snan()
{
    return binary_cast<float>(SNaN);
}

inline float FP<float>::qnan()
{
    return binary_cast<float>(QNaN);
}

inline float FP<float>::indefinite()
{
    return binary_cast<float>(QNaN);
}


//
// FP<double> class implementation.
//

inline uint64 FP<double>::sign(double x)
{
    return (binary_cast<uint64>(x) & 0x8000000000000000LL) >> 63;
}

inline uint64 FP<double>::exponent(double x)
{
    return (binary_cast<uint64>(x) >> 52) & 2047;
}

inline uint64 FP<double>::mantissa(double x)
{
    return binary_cast<uint64>(x) & 0x000FFFFFFFFFFFFFLL;
}

inline bool FP<double>::is_normal(double x)
{
    return exponent(x) > 0 && exponent(x) < 2047;
}

inline bool FP<double>::is_subnormal(double x)
{
    return exponent(x) == 0 && mantissa(x) != 0;
}

inline bool FP<double>::is_zero(double x)
{
    return (binary_cast<uint64>(x) & 0x7FFFFFFFFFFFFFFFLL) == 0;
}

inline bool FP<double>::is_pos_zero(double x)
{
    return binary_cast<uint64>(x) == PosZero;
}

inline bool FP<double>::is_neg_zero(double x)
{
    return binary_cast<uint64>(x) == NegZero;
}

inline bool FP<double>::is_inf(double x)
{
    return (binary_cast<uint64>(x) & 0x7FFFFFFFFFFFFFFFLL) == PosInf;
}

inline bool FP<double>::is_pos_inf(double x)
{
    return binary_cast<uint64>(x) == PosInf;
}

inline bool FP<double>::is_neg_inf(double x)
{
    return binary_cast<uint64>(x) == NegInf;
}

inline bool FP<double>::is_nan(double x)
{
    return exponent(x) == 2047 && mantissa(x) != 0;
}

inline bool FP<double>::is_snan(double x)
{
    return sign(x) == 0 && is_nan(x);
}

inline bool FP<double>::is_qnan(double x)
{
    return sign(x) == 1 && is_nan(x);
}

inline double FP<double>::construct(
    uint64 sign,
    uint64 exponent,
    uint64 mantissa)
{
    assert(sign < 2);
    assert(exponent < (uint64(1) << 11));
    assert(mantissa < (uint64(1) << 52));

    const uint64 value =
          (sign     << 63)
        | (exponent << 52)
        |  mantissa;

    return binary_cast<double>(value);
}

inline double FP<double>::pos_zero()
{
    return binary_cast<double>(PosZero);
}

inline double FP<double>::neg_zero()
{
    return binary_cast<double>(NegZero);
}

inline double FP<double>::pos_min()
{
    return binary_cast<double>(PosMin);
}

inline double FP<double>::neg_min()
{
    return binary_cast<double>(NegMin);
}

inline double FP<double>::pos_inf()
{
    return binary_cast<double>(PosInf);
}

inline double FP<double>::neg_inf()
{
    return binary_cast<double>(NegInf);
}

inline double FP<double>::snan()
{
    return binary_cast<double>(SNaN);
}

inline double FP<double>::qnan()
{
    return binary_cast<double>(QNaN);
}

inline double FP<double>::indefinite()
{
    return binary_cast<double>(QNaN);
}


//
// shift() function implementation.
//

template <typename T>
inline T shift(const T x, const typename TypeConv<T>::Int n)
{
    // Unsigned integer type whose size is equivalent to the size of T.
    typedef typename TypeConv<T>::UInt UInt;

    // Consider the binary representation of x.
    UInt ix = binary_cast<UInt>(x);

    // x is +0.0 or -0.0.
    if ((ix & FP<T>::AbsMask) == 0)
    {
        // Shift toward -infinity.
        if (n < 0)
        {
            ix = FP<T>::NegMin + UInt(-n) - 1;
        }
        // Shift toward +infinity.
        else if (n > 0)
        {
            ix = FP<T>::PosMin + UInt(n) - 1;
        }
    }
    // x is strictly positive.
    else if (ix < FP<T>::NegZero)
    {
        // Shift toward -infinity.
        if (n < 0)
        {
            // Sign change (positive to negative).
            if (ix < FP<T>::PosMin + UInt(-n))          // if shift(x, n) < 0
            {
                ix = FP<T>::PosMin + UInt(-n) - ix + FP<T>::NegMin - 1;
            }
            // No sign change.
            else
            {
                ix -= UInt(-n);
            }
        }
        // Shift toward +infinity.
        else
        {
            // Overflow.
            if (ix > FP<T>::PosInf - UInt(n))           // if shift(x, n) > +inf
            {
                ix = FP<T>::PosInf;
            }
            // No overflow.
            else
            {
                ix += UInt(n);
            }
        }
    }
    // x is strictly negative.
    else
    {
        // Shift toward -infinity.
        if (n < 0)
        {
            // Underflow.
            if (ix > FP<T>::NegInf - UInt(-n))          // if shift(x, n) < -inf
            {
                ix = FP<T>::NegInf;
            }
            // No underflow.
            else
            {
                ix += UInt(-n);
            }
        }
        // Shift toward +infinity.
        else
        {
            // Sign change (negative to positive).
            if (ix < FP<T>::NegMin + UInt(n))           // if shift(x, n) > 0
            {
                ix = FP<T>::NegMin + UInt(n) - ix + FP<T>::PosMin - 1;
            }
            // No sign change.
            else
            {
                ix -= UInt(n);
            }
        }
    }

    // Return the new value of x as a scalar.
    return binary_cast<T>(ix);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FP_H
