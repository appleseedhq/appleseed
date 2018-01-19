
// This code was taken from Ptex with minimal changes.
// Original license follows:

/*
PTEX SOFTWARE
Copyright 2014 Disney Enterprises, Inc.  All rights reserved

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

  * The names "Disney", "Walt Disney Pictures", "Walt Disney Animation
    Studios" or the names of its contributors may NOT be used to
    endorse or promote products derived from this software without
    specific prior written permission from Walt Disney Pictures.

Disclaimer: THIS SOFTWARE IS PROVIDED BY WALT DISNEY PICTURES AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE, NONINFRINGEMENT AND TITLE ARE DISCLAIMED.
IN NO EVENT SHALL WALT DISNEY PICTURES, THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND BASED ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/

#ifndef APPLESEED_FOUNDATION_MATH_HALF_H
#define APPLESEED_FOUNDATION_MATH_HALF_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// Half-precision (16-bit) floating-point type.
//
// This type should be compatible with opengl, openexr, and IEEE 754r.
// The range is [-65504.0, 65504.0] and the precision is about 1 part
// in 2000 (3.3 decimal places).
//
// From OpenGL spec 2.1.2:
//
// A 16-bit floating-point number has a 1-bit sign (S), a 5-bit
// exponent (E), and a 10-bit mantissa (M).  The value of a 16-bit
// floating-point number is determined by the following:
//
// (-1)^S * 0.0,                        if E == 0 and M == 0,
// (-1)^S * 2^-14 * (M/2^10),           if E == 0 and M != 0,
// (-1)^S * 2^(E-15) * (1 + M/2^10),    if 0 < E < 31,
// (-1)^S * INF,                        if E == 31 and M == 0, or
// NaN,                                 if E == 31 and M != 0 \endverbatim
//

class Half
{
  public:

    Half();

    Half(const float val);

    Half& operator=(const float val);

    operator float() const;

    uint16_t bits() const;
    void set_bits(const uint16_t bits);

  private:

    uint16_t m_bits;

    static uint16_t from_float(const float val);

    APPLESEED_DLLSYMBOL static uint16_t from_float_except(const uint32_t val);

    APPLESEED_DLLSYMBOL static const uint32_t m_h2f_table[65536];
    APPLESEED_DLLSYMBOL static const uint16_t m_f2h_table[512];
};

//
// Half class implementation.
//

inline Half::Half() {}

inline Half::Half(const float val)
  : m_bits(from_float(val))
{
}

inline Half& Half::operator=(const float val)
{
    m_bits = from_float(val);
    return *this;
}

inline Half::operator float() const
{
    union { uint32_t i; float f; } u;
    u.i = m_h2f_table[m_bits];
    return u.f;
}

inline uint16_t Half::bits() const
{
    return m_bits;
}

inline void Half::set_bits(const uint16_t bits)
{
    m_bits = bits;
}

inline uint16_t Half::from_float(const float val)
{
    if (val == 0.0f)
        return 0;

    union { uint32_t i; float f; } u;
    u.f = val;
    int e = m_f2h_table[(u.i >> 23) & 0x1ff];

    if (e)
        return static_cast<uint16_t>(e + (((u.i & 0x7fffff) + 0x1000) >> 13));

    return from_float_except(u.i);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_HALF_H
