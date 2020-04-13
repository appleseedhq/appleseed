
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cmath>
#include <cstdint>

namespace foundation
{

//
// The CompressedUnitVector class stores unit vectors
// encoded as 32-bit octahedral unit vectors.
//
// Reference:
//
//   A Survey of Efficient Representations for Independent Unit Vectors.
//   http://jcgt.org/published/0003/02/01/paper-lowres.pdf
//

class CompressedUnitVector
{
  public:
    typedef std::int16_t ValueType;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    CompressedUnitVector() = default;     // leave uninitialized
#else
    CompressedUnitVector() {}             // leave uninitialized
#endif

    explicit CompressedUnitVector(const Vector3f& vec);

    operator Vector3f() const;

    bool operator==(const CompressedUnitVector& rhs) const;
    bool operator!=(const CompressedUnitVector& rhs) const;

  private:
    static float sign_not_zero(const float v);
    static ValueType round16(const float f);
    static ValueType floor16(const float f);
    static Vector3f oct_decode(const ValueType bits[2]);

    ValueType m_bits[2];
};


//
// CompressedUnitVector class implementation.
//

inline CompressedUnitVector::CompressedUnitVector(const Vector3f& vec)
{
    ValueType projected[2];

    const float inv_l1_norm = 1.0f / (std::abs(vec[0]) + std::abs(vec[1]) + std::abs(vec[2]));

    if (vec[2] <= 0.0f)
    {
        projected[0] = floor16((1.0f - std::abs(vec[1] * inv_l1_norm)) * sign_not_zero(vec[0]));
        projected[1] = floor16((1.0f - std::abs(vec[0] * inv_l1_norm)) * sign_not_zero(vec[1]));
    }
    else
    {
        projected[0] = floor16(vec[0] * inv_l1_norm);
        projected[1] = floor16(vec[1] * inv_l1_norm);
    }

    ValueType best_projected[2] = {0, 0};
    float error = 0.0f;

    unsigned int bits_x = static_cast<unsigned int>(projected[0]);
    unsigned int bits_y = static_cast<unsigned int>(projected[1]);

    for (unsigned int i = 0; i < 2; ++i)
    {
        for (unsigned int j = 0; j < 2; ++j)
        {
            projected[0] = static_cast<ValueType>(bits_x + i);
            projected[1] = static_cast<ValueType>(bits_y + j);

            const Vector3f decoded = oct_decode(projected);

            const float alt_error = std::abs(dot(vec, decoded));
            if (alt_error > error)
            {
                error = alt_error;
                best_projected[0] = projected[0];
                best_projected[1] = projected[1];
            }
        }
    }

    m_bits[0] = best_projected[0];
    m_bits[1] = best_projected[1];
}

inline CompressedUnitVector::operator Vector3f() const
{
    return oct_decode(m_bits);
}

inline bool CompressedUnitVector::operator==(const CompressedUnitVector& rhs) const
{
    return m_bits[0] == rhs.m_bits[0] && m_bits[1] == rhs.m_bits[1];
}

inline bool CompressedUnitVector::operator!=(const CompressedUnitVector& rhs) const
{
    return !(*this == rhs);
}

inline float CompressedUnitVector::sign_not_zero(const float v)
{
    return v < 0.0f ? -1.0f : 1.0f;
}

inline CompressedUnitVector::ValueType CompressedUnitVector::round16(const float f)
{
    return round<ValueType>(clamp(f, -1.0f, 1.0f) * 32767.0f);
}

inline CompressedUnitVector::ValueType CompressedUnitVector::floor16(const float f)
{
    return static_cast<ValueType>(std::floor(clamp(f, -1.0f, 1.0f) * 32767.0f));
}

inline Vector3f CompressedUnitVector::oct_decode(const ValueType bits[2])
{
    Vector3f vec;

    const float Rcp32767 = 1.0f / 32767.0f;
    vec[0] = clamp(bits[0] * Rcp32767, -1.0f, 1.0f);
    vec[1] = clamp(bits[1] * Rcp32767, -1.0f, 1.0f);
    vec[2] = 1.0f - (std::abs(vec[0]) + std::abs(vec[1]));

    if (vec[2] < 0.0f)
    {
        const float tmp = vec[0];
        vec[0] = (1.0f - std::abs(vec[1])) * sign_not_zero(tmp);
        vec[1] = (1.0f - std::abs(tmp)) * sign_not_zero(vec[1]);
    }

    return normalize(vec);
}

}       // namespace foundation
