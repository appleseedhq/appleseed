
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/utility/casts.h"
#include "foundation/utility/typetraits.h"

// Standard headers.
#include <cassert>
#include <cstdint>

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
    static const std::uint32_t PosZero = 0x00000000u;       // +0.0
    static const std::uint32_t NegZero = 0x80000000u;       // -0.0
    static const std::uint32_t PosMin  = 0x00800000u;       // +1.175494351e-38f
    static const std::uint32_t NegMin  = 0x80800000u;       // -1.175494351e-38f
    static const std::uint32_t PosInf  = 0x7F800000u;       // +infinity
    static const std::uint32_t NegInf  = 0xFF800000u;       // -infinity
    static const std::uint32_t SNaN    = 0x7FFFFFFFu;       // signaling NaN
    static const std::uint32_t QNaN    = 0xFFFFFFFFu;       // quiet NaN (indefinite)
    static const std::uint32_t Ind     = QNaN;              // synonym for QNaN

    // Useful bitmasks.
    static const std::uint32_t AbsMask = 0x7FFFFFFFu;       // absolute value bitmask

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
    static std::uint32_t sign(const float x);

    // Extract the exponent bits from a floating-point number.
    static std::uint32_t exponent(const float x);

    // Extract the mantissa bits from a floating-point number.
    static std::uint32_t mantissa(const float x);

    // Return true if x is a normalized number.
    static bool is_normal(const float x);

    // Return true if x is a subnormal (denormalized) number.
    static bool is_subnormal(const float x);

    // Return true if x equals +0.0 or -0.0.
    static bool is_zero(const float x);

    // Return true if x equals +0.0.
    static bool is_pos_zero(const float x);

    // Return true if x equals -0.0.
    static bool is_neg_zero(const float x);

    // Return true if x equals +infinity or -infinity.
    static bool is_inf(const float x);

    // Return true if x equals +infinity.
    static bool is_pos_inf(const float x);

    // Return true if x equals -infinity.
    static bool is_neg_inf(const float x);

    // Return true if x equals signaling NaN or quiet NaN (indefinite).
    static bool is_nan(const float x);

    // Return true if x equals signaling NaN.
    static bool is_snan(const float x);

    // Return true if x equals quiet NaN.
    static bool is_qnan(const float x);

    // Return true if x is neither NaN nor infinite.
    static bool is_finite(const float x);

    // Return true if x is neither NaN nor infinite nor negative.
    static bool is_finite_non_neg(const float x);

    // Construct a 32-bit floating-point value given a sign, an exponent and a mantissa.
    static float construct(
        const std::uint32_t sign,
        const std::uint32_t exponent,
        const std::uint32_t mantissa);
};

template <>
struct FP<double>
{
    // Binary representation of special 64-bit floating-point values.
    static const std::uint64_t PosZero = 0x0000000000000000ull;     // +0.0
    static const std::uint64_t NegZero = 0x8000000000000000ull;     // -0.0
    static const std::uint64_t PosMin  = 0x0080000000000000ull;     // +1.175494351e-38f
    static const std::uint64_t NegMin  = 0x8080000000000000ull;     // -1.175494351e-38f
    static const std::uint64_t PosInf  = 0x7FF0000000000000ull;     // +infinity
    static const std::uint64_t NegInf  = 0xFFF0000000000000ull;     // -infinity
    static const std::uint64_t SNaN    = 0x7FFFFFFFFFFFFFFFull;     // signaling NaN
    static const std::uint64_t QNaN    = 0xFFFFFFFFFFFFFFFFull;     // quiet NaN (indefinite)
    static const std::uint64_t Ind     = QNaN;                      // synonym for QNaN

    // Useful bitmasks.
    static const std::uint64_t AbsMask = 0x7FFFFFFFFFFFFFFFull;     // absolute value bitmask

    // Return special 64-bit floating-point values.
    static double pos_zero();                                       // +0.0
    static double neg_zero();                                       // -0.0
    static double pos_min();                                        // +1.175494351e-38f
    static double neg_min();                                        // -1.175494351e-38f
    static double pos_inf();                                        // +infinity
    static double neg_inf();                                        // -infinity
    static double snan();                                           // signaling NaN
    static double qnan();                                           // quiet NaN (indefinite)
    static double indefinite();                                     // synonym for qnan()

    // Extract the sign bit from a floating-point number.
    static std::uint64_t sign(const double x);

    // Extract the exponent bits from a floating-point number.
    static std::uint64_t exponent(const double x);

    // Extract the mantissa bits from a floating-point number.
    static std::uint64_t mantissa(const double x);

    // Return true if x is a normalized number.
    static bool is_normal(const double x);

    // Return true if x is a subnormal (denormalized) number.
    static bool is_subnormal(const double x);

    // Return true if x equals +0.0 or -0.0.
    static bool is_zero(const double x);

    // Return true if x equals +0.0.
    static bool is_pos_zero(const double x);

    // Return true if x equals -0.0.
    static bool is_neg_zero(const double x);

    // Return true if x equals +infinity or -infinity.
    static bool is_inf(const double x);

    // Return true if x equals +infinity.
    static bool is_pos_inf(const double x);

    // Return true if x equals -infinity.
    static bool is_neg_inf(const double x);

    // Return true if x equals signaling NaN or quiet NaN (indefinite).
    static bool is_nan(const double x);

    // Return true if x equals signaling NaN.
    static bool is_snan(const double x);

    // Return true if x equals quiet NaN.
    static bool is_qnan(const double x);

    // Return true if x is neither NaN nor infinite.
    static bool is_finite(const double x);

    // Return true if x is neither NaN nor infinite nor negative.
    static bool is_finite_non_neg(const double x);

    // Construct a 64-bit floating-point value given a sign, an exponent and a mantissa.
    static double construct(
        const std::uint64_t sign,
        const std::uint64_t exponent,
        const std::uint64_t mantissa);
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

inline std::uint32_t FP<float>::sign(const float x)
{
    return (binary_cast<std::uint32_t>(x) & 0x80000000u) >> 31;
}

inline std::uint32_t FP<float>::exponent(const float x)
{
    return (binary_cast<std::uint32_t>(x) >> 23) & 255;
}

inline std::uint32_t FP<float>::mantissa(const float x)
{
    return binary_cast<std::uint32_t>(x) & 0x007FFFFFu;
}

inline bool FP<float>::is_normal(const float x)
{
    return exponent(x) > 0 && exponent(x) < 255;
}

inline bool FP<float>::is_subnormal(const float x)
{
    return exponent(x) == 0 && mantissa(x) != 0;
}

inline bool FP<float>::is_zero(const float x)
{
    return (binary_cast<std::uint32_t>(x) & 0x7FFFFFFFu) == 0;
}

inline bool FP<float>::is_pos_zero(const float x)
{
    return binary_cast<std::uint32_t>(x) == PosZero;
}

inline bool FP<float>::is_neg_zero(const float x)
{
    return binary_cast<std::uint32_t>(x) == NegZero;
}

inline bool FP<float>::is_inf(const float x)
{
    return (binary_cast<std::uint32_t>(x) & 0x7FFFFFFFu) == PosInf;
}

inline bool FP<float>::is_pos_inf(const float x)
{
    return binary_cast<std::uint32_t>(x) == PosInf;
}

inline bool FP<float>::is_neg_inf(const float x)
{
    return binary_cast<std::uint32_t>(x) == NegInf;
}

inline bool FP<float>::is_nan(const float x)
{
    return exponent(x) == 255 && mantissa(x) != 0;
}

inline bool FP<float>::is_snan(const float x)
{
    return sign(x) == 0 && is_nan(x);
}

inline bool FP<float>::is_qnan(const float x)
{
    return sign(x) == 1 && is_nan(x);
}

inline bool FP<float>::is_finite(const float x)
{
    return !is_inf(x) && !is_nan(x);
}

inline bool FP<float>::is_finite_non_neg(const float x)
{
    const std::uint32_t ix = binary_cast<std::uint32_t>(x);
    const std::uint32_t sign = (ix & 0x80000000u) >> 31;
    const std::uint32_t exponent = (ix >> 23) & 255;
    const std::uint32_t mantissa = ix & 0x007FFFFFu;
    const bool is_neg = sign == 1 && ix != 0x80000000u;
    const bool is_nan = exponent == 255 && mantissa != 0;
    const bool is_inf = (ix & 0x7FFFFFFFu) == PosInf;
    return !is_neg && !is_nan && !is_inf;
}

inline float FP<float>::construct(
    const std::uint32_t    sign,
    const std::uint32_t    exponent,
    const std::uint32_t    mantissa)
{
    assert(sign < 2);
    assert(exponent < (std::uint32_t(1) << 8));
    assert(mantissa < (std::uint32_t(1) << 23));

    const std::uint32_t value =
          (sign     << 31)
        | (exponent << 23)
        |  mantissa;

    return binary_cast<float>(value);
}


//
// FP<double> class implementation.
//

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

inline std::uint64_t FP<double>::sign(const double x)
{
    return (binary_cast<std::uint64_t>(x) & 0x8000000000000000ull) >> 63;
}

inline std::uint64_t FP<double>::exponent(const double x)
{
    return (binary_cast<std::uint64_t>(x) >> 52) & 2047;
}

inline std::uint64_t FP<double>::mantissa(const double x)
{
    return binary_cast<std::uint64_t>(x) & 0x000FFFFFFFFFFFFFull;
}

inline bool FP<double>::is_normal(const double x)
{
    return exponent(x) > 0 && exponent(x) < 2047;
}

inline bool FP<double>::is_subnormal(const double x)
{
    return exponent(x) == 0 && mantissa(x) != 0;
}

inline bool FP<double>::is_zero(const double x)
{
    return (binary_cast<std::uint64_t>(x) & 0x7FFFFFFFFFFFFFFFull) == 0;
}

inline bool FP<double>::is_pos_zero(const double x)
{
    return binary_cast<std::uint64_t>(x) == PosZero;
}

inline bool FP<double>::is_neg_zero(const double x)
{
    return binary_cast<std::uint64_t>(x) == NegZero;
}

inline bool FP<double>::is_inf(const double x)
{
    return (binary_cast<std::uint64_t>(x) & 0x7FFFFFFFFFFFFFFFull) == PosInf;
}

inline bool FP<double>::is_pos_inf(const double x)
{
    return binary_cast<std::uint64_t>(x) == PosInf;
}

inline bool FP<double>::is_neg_inf(const double x)
{
    return binary_cast<std::uint64_t>(x) == NegInf;
}

inline bool FP<double>::is_nan(const double x)
{
    return exponent(x) == 2047 && mantissa(x) != 0;
}

inline bool FP<double>::is_snan(const double x)
{
    return sign(x) == 0 && is_nan(x);
}

inline bool FP<double>::is_qnan(const double x)
{
    return sign(x) == 1 && is_nan(x);
}

inline bool FP<double>::is_finite(const double x)
{
    return !is_inf(x) && !is_nan(x);
}

inline bool FP<double>::is_finite_non_neg(const double x)
{
    const std::uint64_t ix = binary_cast<std::uint64_t>(x);
    const std::uint64_t sign = (ix & 0x8000000000000000ull) >> 63;
    const std::uint64_t exponent = (ix >> 52) & 2047;
    const std::uint64_t mantissa = ix & 0x000FFFFFFFFFFFFFull;
    const bool is_neg = sign == 1 && ix != 0x8000000000000000ull;
    const bool is_nan = exponent == 2047 && mantissa != 0;
    const bool is_inf = (ix & 0x7FFFFFFFFFFFFFFFull) == PosInf;
    return !is_neg && !is_nan && !is_inf;
}

inline double FP<double>::construct(
    const std::uint64_t    sign,
    const std::uint64_t    exponent,
    const std::uint64_t    mantissa)
{
    assert(sign < 2);
    assert(exponent < (std::uint64_t(1) << 11));
    assert(mantissa < (std::uint64_t(1) << 52));

    const std::uint64_t value =
          (sign     << 63)
        | (exponent << 52)
        |  mantissa;

    return binary_cast<double>(value);
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

}   // namespace foundation
