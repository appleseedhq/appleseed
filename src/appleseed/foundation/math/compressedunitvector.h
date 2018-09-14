
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

#ifndef APPLESEED_FOUNDATION_MATH_COMPRESSED_UNIT_VECTOR_H
#define APPLESEED_FOUNDATION_MATH_COMPRESSED_UNIT_VECTOR_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cmath>

namespace foundation
{

//
// The CompressedUnitVector class stores unit vectors
// encoded as 32 bits octahedral unit vectors.
//
// Reference:
//
//   A Survey of Efficient Representations for Independent Unit Vectors.
//   http://jcgt.org/published/0003/02/01/paper-lowres.pdf
//

class CompressedUnitVector
{
  public:

    // Constructors.
#if !defined(_MSC_VER) || _MSC_VER >= 1800
    CompressedUnitVector() = default;     // leave uninitialized
#else
    CompressedUnitVector() {}             // leave uninitialized
#endif

    CompressedUnitVector(const foundation::Vector3f& vec);

    operator foundation::Vector3f() const;

    bool operator==(const CompressedUnitVector& rhs) const;
    bool operator!=(const CompressedUnitVector& rhs) const;

  private:
    static int16 round16(const float f);
    static int16 floor16(const float f);
    static foundation::Vector3f oct_decode(const int16 bits[2]);

    int16 m_bits[2];
};


//
// CompressedUnitVector class implementation.
//

inline CompressedUnitVector::CompressedUnitVector(const foundation::Vector3f& vec)
{
    int16 projected[2];

    const float inv_l1_norm = 1.0f / (std::fabs(vec[0]) + std::fabs(vec[1]) + std::fabs(vec[2]));

    if (vec[2] <= 0.0f)
    {
        projected[0] = floor16((1.0f - std::fabs(vec[1] * inv_l1_norm)) * std::copysign(1.0f, vec[0]));
        projected[1] = floor16((1.0f - std::fabs(vec[0] * inv_l1_norm)) * std::copysign(1.0f, vec[1]));
    }
    else
    {
        projected[0] = floor16(vec[0] * inv_l1_norm);
        projected[1] = floor16(vec[1] * inv_l1_norm);
    }

    int16 best_projected[2] = {0, 0};
    float error = 0.0f;

    unsigned int bits_x = static_cast<unsigned int>(projected[0]);
    unsigned int bits_y = static_cast<unsigned int>(projected[1]);

    for (int i = 0; i < 2; ++i)
    {
        for (int j = 0; j< 2; ++j)
        {
            projected[0] = bits_x + i;
            projected[1] = bits_y + j;

            foundation::Vector3f decoded = oct_decode(projected);

            const float alt_error = std::fabs(dot(vec, decoded));
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


inline CompressedUnitVector::operator foundation::Vector3f() const
{
    return oct_decode(m_bits);
}

inline bool CompressedUnitVector::operator==(const CompressedUnitVector& rhs) const
{
    return m_bits[0] == rhs.m_bits[0] && m_bits[1] == rhs.m_bits[1];
}

inline bool CompressedUnitVector::operator!=(const CompressedUnitVector& rhs) const
{
    return m_bits[0] != rhs.m_bits[0] || m_bits[1] != rhs.m_bits[1];
}

inline int16 CompressedUnitVector::round16(const float f)
{
    return std::round(clamp(f, -1.0f, 1.0f) * 32767.0f);
}

inline int16 CompressedUnitVector::floor16(const float f)
{
    return std::floor(clamp(f, -1.0f, 1.0f) * 32767.0f);
}

inline foundation::Vector3f CompressedUnitVector::oct_decode(const int16 bits[2])
{
    foundation::Vector3f vec;

    const float rcp_32767 = 1.0f / 32767.0f;
    vec[0] = clamp(bits[0] * rcp_32767, -1.0f, 1.0f);
    vec[1] = clamp(bits[1] * rcp_32767, -1.0f, 1.0f);
    vec[2] = 1.0f - (std::fabs(vec[0]) + std::fabs(vec[1]));

    if (vec[2] < 0.0f)
    {
        const float tmp = vec[0];
        vec[0] = (1.0f - std::fabs(vec[1])) * std::copysign(1.0f, tmp);
        vec[1] = (1.0f - std::fabs(tmp))   * std::copysign(1.0f, vec[1]);
    }

    return normalize(vec);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_COMPRESSED_UNIT_VECTOR_H
