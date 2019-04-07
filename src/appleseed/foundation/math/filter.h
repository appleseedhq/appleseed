
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
#include "foundation/math/fastmath.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// Base class for 1D reconstruction filters.
//
// The filters are not normalized (they don't integrate to 1 over their domain).
// The return value of evaluate() is undefined if (x) is outside the filter's domain.
// Filters that have a closed form sampling formula implement a sample method.
//

template <typename T>
class Filter1
{
  public:
    typedef T ValueType;

    explicit Filter1(const T radius);

    T get_radius() const;

  protected:
    const T m_radius;
    const T m_rcp_radius;
};


//
// 1D box filter.
//

template <typename T>
class BoxFilter1
  : public Filter1<T>
{
  public:
    explicit BoxFilter1(const T radius);

    T evaluate(const T x) const;

    T sample(const T s) const;
};


//
// 1D triangle filter.
//

template <typename T>
class TriangleFilter1
  : public Filter1<T>
{
  public:
    explicit TriangleFilter1(const T radius);

    T evaluate(const T x) const;

    T sample(const T s) const;

  private:
    static T sample_linear(const T a, const T b, const T s);
};


//
// 1D Gaussian filter.
//

template <typename T>
class GaussianFilter1
  : public Filter1<T>
{
  public:
    GaussianFilter1(
        const T radius,
        const T alpha);

    T evaluate(const T x) const;

  private:
    const T m_alpha;
    const T m_shift;

    static T gaussian(const T x, const T alpha);
};


//
// 1D Blackman-Harris filter.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Window_function#Higher-order_generalized_cosine_windows
//

template <typename T>
class BlackmanHarrisFilter1
  : public Filter1<T>
{
  public:
    explicit BlackmanHarrisFilter1(const T radius);

    T evaluate(const T x) const;
};


//
// Base class for 2D reconstruction filters.
//
// The filters are not normalized (they don't integrate to 1 over their domain).
// The return value of evaluate() is undefined if (x, y) is outside the filter's domain.
//

template <typename T>
class Filter2
{
  public:
    typedef T ValueType;

    Filter2(const T xradius, const T yradius);

    virtual ~Filter2() {}

    T get_xradius() const;
    T get_yradius() const;

    virtual T evaluate(const T x, const T y) const = 0;

  protected:
    const T m_xradius;
    const T m_yradius;
    const T m_rcp_xradius;
    const T m_rcp_yradius;
};


//
// Specializations.
//

typedef Filter2<float>  Filter2f;
typedef Filter2<double> Filter2d;


//
// 2D box filter.
//

template <typename T>
class BoxFilter2
  : public Filter2<T>
{
  public:
    BoxFilter2(const T xradius, const T yradius);

    T evaluate(const T x, const T y) const override;
};


//
// 2D triangle filter.
//

template <typename T>
class TriangleFilter2
  : public Filter2<T>
{
  public:
    TriangleFilter2(const T xradius, const T yradius);

    T evaluate(const T x, const T y) const override;
};


//
// 2D Gaussian filter.
//

template <typename T>
class GaussianFilter2
  : public Filter2<T>
{
  public:
    GaussianFilter2(
        const T xradius,
        const T yradius,
        const T alpha);

    T evaluate(const T x, const T y) const override;

  private:
    const T m_alpha;
    const T m_shift;

    static T gaussian(const T x, const T alpha);
};

template <typename T>
class FastGaussianFilter2
  : public Filter2<T>
{
  public:
    FastGaussianFilter2(
        const T xradius,
        const T yradius,
        const T alpha);

    T evaluate(const T x, const T y) const override;

  private:
    const T m_alpha;
    const T m_shift;

    static T gaussian(const T x, const T alpha);
};


//
// 2D Mitchell-Netravali filter.
//
// Reference:
//
//   http://www.cs.utexas.edu/~fussell/courses/cs384g/lectures/mitchell/Mitchell.pdf
//

template <typename T>
class MitchellFilter2
  : public Filter2<T>
{
  public:
    MitchellFilter2(
        const T xradius,
        const T yradius,
        const T b,
        const T c);

    T evaluate(const T x, const T y) const override;

  private:
    T m_a3, m_a2, m_a0;
    T m_b3, m_b2, m_b1, m_b0;
};


//
// 2D Lanczos filter.
//

template <typename T>
class LanczosFilter2
  : public Filter2<T>
{
  public:
    LanczosFilter2(
        const T xradius,
        const T yradius,
        const T tau);

    T evaluate(const T x, const T y) const override;

  private:
    const T m_rcp_tau;

    static T lanczos(const T x, const T rcp_tau);
    static T sinc(const T x);
};


//
// 2D Blackman-Harris filter.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Window_function#Higher-order_generalized_cosine_windows
//

template <typename T>
class BlackmanHarrisFilter2
  : public Filter2<T>
{
  public:
    BlackmanHarrisFilter2(const T xradius, const T yradius);

    T evaluate(const T x, const T y) const override;

  private:
    static T blackman(const T x);
};

template <typename T>
class FastBlackmanHarrisFilter2
  : public Filter2<T>
{
  public:
    FastBlackmanHarrisFilter2(const T xradius, const T yradius);

    T evaluate(const T x, const T y) const override;

  private:
    static T blackman(const T x);
};


//
// Filter1 class implementation.
//

template <typename T>
inline Filter1<T>::Filter1(const T radius)
  : m_radius(radius)
  , m_rcp_radius(T(1.0) / radius)
{
}

template <typename T>
inline T Filter1<T>::get_radius() const
{
    return m_radius;
}


//
// BoxFilter1 class implementation.
//

template <typename T>
inline BoxFilter1<T>::BoxFilter1(const T radius)
  : Filter1<T>(radius)
{
}

template <typename T>
inline T BoxFilter1<T>::evaluate(const T x) const
{
    return T(1.0);
}

template <typename T>
inline T BoxFilter1<T>::sample(const T s) const
{
    const T r = Filter1<T>::m_radius;
    return lerp(-r, r, s);
}


//
// TriangleFilter1 class implementation.
//

template <typename T>
inline TriangleFilter1<T>::TriangleFilter1(const T radius)
  : Filter1<T>(radius)
{
}

template <typename T>
inline T TriangleFilter1<T>::evaluate(const T x) const
{
    const T nx = x * Filter1<T>::m_rcp_radius;
    return T(1.0) - std::abs(nx);
}

template <typename T>
inline T TriangleFilter1<T>::sample(const T s) const
{
    const T r = Filter1<T>::m_radius;
    return s < T(0.5)
        ? -r * sample_linear(T(1.0), T(0.0), T(2.0) * s)
        :  r * sample_linear(T(1.0), T(0.0), T(2.0) * (s - T(0.5)));

}

template <typename T>
inline T TriangleFilter1<T>::sample_linear(const T a, const T b, const T s)
{
    return saturate((a - std::sqrt(lerp(square(a), square(b), s))) / (a - b));
}


//
// GaussianFilter1 class implementation.
//

template <typename T>
inline GaussianFilter1<T>::GaussianFilter1(
    const T radius,
    const T alpha)
  : Filter1<T>(radius)
  , m_alpha(alpha)
  , m_shift(gaussian(T(1.0), alpha))
{
}

template <typename T>
inline T GaussianFilter1<T>::evaluate(const T x) const
{
    const T nx = x * Filter1<T>::m_rcp_radius;
    return gaussian(nx, m_alpha) - m_shift;
}

template <typename T>
inline T GaussianFilter1<T>::gaussian(const T x, const T alpha)
{
    return std::exp(-alpha * x * x);
}


//
// BlackmanHarrisFilter1 class implementation.
//

template <typename T>
inline BlackmanHarrisFilter1<T>::BlackmanHarrisFilter1(const T radius)
  : Filter1<T>(radius)
{
}

template <typename T>
inline T BlackmanHarrisFilter1<T>::evaluate(const T x) const
{
    const T nx = T(0.5) * (T(1.0) + x * Filter1<T>::m_rcp_radius);

    return
        T(0.35875)
      - T(0.48829) * std::cos((T(2.0) * Pi<T>()) * nx)
      + T(0.14128) * std::cos((T(4.0) * Pi<T>()) * nx)
      - T(0.01174) * std::cos((T(6.0) * Pi<T>()) * nx); // original coefficient is 0.01168, modified to ensure 0 at borders
}


//
// Utilities.
//

// Compute the normalization factor for a given filter.
template <typename Filter>
typename Filter::ValueType compute_normalization_factor(
    const Filter&   filter,
    const size_t    sample_count = 1024);


//
// Filter2 class implementation.
//

template <typename T>
inline Filter2<T>::Filter2(const T xradius, const T yradius)
  : m_xradius(xradius)
  , m_yradius(yradius)
  , m_rcp_xradius(T(1.0) / xradius)
  , m_rcp_yradius(T(1.0) / yradius)
{
}

template <typename T>
inline T Filter2<T>::get_xradius() const
{
    return m_xradius;
}

template <typename T>
inline T Filter2<T>::get_yradius() const
{
    return m_yradius;
}


//
// BoxFilter2 class implementation.
//

template <typename T>
inline BoxFilter2<T>::BoxFilter2(const T xradius, const T yradius)
  : Filter2<T>(xradius, yradius)
{
}

template <typename T>
inline T BoxFilter2<T>::evaluate(const T x, const T y) const
{
    return T(1.0);
}


//
// TriangleFilter2 class implementation.
//

template <typename T>
inline TriangleFilter2<T>::TriangleFilter2(const T xradius, const T yradius)
  : Filter2<T>(xradius, yradius)
{
}

template <typename T>
inline T TriangleFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = x * Filter2<T>::m_rcp_xradius;
    const T ny = y * Filter2<T>::m_rcp_yradius;
    return (T(1.0) - std::abs(nx)) * (T(1.0) - std::abs(ny));
}


//
// GaussianFilter2 class implementation.
//

template <typename T>
inline GaussianFilter2<T>::GaussianFilter2(
    const T xradius,
    const T yradius,
    const T alpha)
  : Filter2<T>(xradius, yradius)
  , m_alpha(alpha)
  , m_shift(gaussian(T(1.0), alpha))
{
}

template <typename T>
inline T GaussianFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = x * Filter2<T>::m_rcp_xradius;
    const T ny = y * Filter2<T>::m_rcp_yradius;

    const T fx = gaussian(nx, m_alpha) - m_shift;
    const T fy = gaussian(ny, m_alpha) - m_shift;

    return fx * fy;
}

template <typename T>
APPLESEED_FORCE_INLINE T GaussianFilter2<T>::gaussian(const T x, const T alpha)
{
    return
        static_cast<T>(
            std::exp(
                static_cast<float>(-alpha * x * x)));
}


//
// FastGaussianFilter2 class implementation.
//

template <typename T>
inline FastGaussianFilter2<T>::FastGaussianFilter2(
    const T xradius,
    const T yradius,
    const T alpha)
  : Filter2<T>(xradius, yradius)
  , m_alpha(alpha)
  , m_shift(gaussian(T(1.0), alpha))
{
}

template <typename T>
inline T FastGaussianFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = x * Filter2<T>::m_rcp_xradius;
    const T ny = y * Filter2<T>::m_rcp_yradius;

    const T fx = gaussian(nx, m_alpha) - m_shift;
    const T fy = gaussian(ny, m_alpha) - m_shift;

    return fx * fy;
}

template <typename T>
APPLESEED_FORCE_INLINE T FastGaussianFilter2<T>::gaussian(const T x, const T alpha)
{
    // Use foundation::fast_exp() because foundation::faster_exp() is way too inaccurate.
    return
        static_cast<T>(
            fast_exp(
                static_cast<float>(-alpha * x * x)));
}


//
// MitchellFilter2 class implementation.
//

template <typename T>
inline MitchellFilter2<T>::MitchellFilter2(
    const T xradius,
    const T yradius,
    const T b,
    const T c)
  : Filter2<T>(xradius, yradius)
{
    m_a3 = T(1.0 / 6.0) * (T(12.0) - T(9.0) * b - T(6.0) * c);
    m_a2 = T(1.0 / 6.0) * (T(-18.0) + T(12.0) * b + T(6.0) * c);
    m_a0 = T(1.0 / 6.0) * (T(6.0) - T(2.0) * b);

    m_b3 = T(1.0 / 6.0) * (-b - T(6.0) * c);
    m_b2 = T(1.0 / 6.0) * (T(6.0) * b + T(30.0) * c);
    m_b1 = T(1.0 / 6.0) * (T(-12.0) * b - T(48.0) * c);
    m_b0 = T(1.0 / 6.0) * (T(8.0) * b + T(24.0) * c);
}

template <typename T>
inline T MitchellFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = x * Filter2<T>::m_rcp_xradius;
    const T x1 = std::abs(nx + nx);
    const T x2 = x1 * x1;
    const T x3 = x2 * x1;

    const T fx =
        x1 < T(1.0)
            ? m_a3 * x3 + m_a2 * x2 + m_a0
            : m_b3 * x3 + m_b2 * x2 + m_b1 * x1 + m_b0;

    const T ny = y * Filter2<T>::m_rcp_yradius;
    const T y1 = std::abs(ny + ny);
    const T y2 = y1 * y1;
    const T y3 = y2 * y1;

    const T fy =
        y1 < T(1.0)
            ? m_a3 * y3 + m_a2 * y2 + m_a0
            : m_b3 * y3 + m_b2 * y2 + m_b1 * y1 + m_b0;

    return fx * fy;
}


//
// LanczosFilter2 class implementation.
//

template <typename T>
inline LanczosFilter2<T>::LanczosFilter2(
    const T xradius,
    const T yradius,
    const T tau)
  : Filter2<T>(xradius, yradius)
  , m_rcp_tau(tau)
{
}

template <typename T>
inline T LanczosFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = x * Filter2<T>::m_rcp_xradius;
    const T ny = y * Filter2<T>::m_rcp_yradius;
    return lanczos(nx, m_rcp_tau) * lanczos(ny, m_rcp_tau);
}

template <typename T>
APPLESEED_FORCE_INLINE T LanczosFilter2<T>::lanczos(const T x, const T rcp_tau)
{
    const T theta = Pi<T>() * x;
    return theta == T(0.0) ? T(1.0) : sinc(theta * rcp_tau) * sinc(theta);
}

template <typename T>
APPLESEED_FORCE_INLINE T LanczosFilter2<T>::sinc(const T x)
{
    return std::sin(x) / x;
}


//
// BlackmanHarrisFilter2 class implementation.
//

template <typename T>
inline BlackmanHarrisFilter2<T>::BlackmanHarrisFilter2(const T xradius, const T yradius)
  : Filter2<T>(xradius, yradius)
{
}

template <typename T>
inline T BlackmanHarrisFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = T(0.5) * (T(1.0) + x * Filter2<T>::m_rcp_xradius);
    const T ny = T(0.5) * (T(1.0) + y * Filter2<T>::m_rcp_yradius);
    return blackman(nx) * blackman(ny);
}

template <typename T>
APPLESEED_FORCE_INLINE T BlackmanHarrisFilter2<T>::blackman(const T x)
{
    return
          T(0.35875)
        - T(0.48829) * std::cos((T(2.0) * Pi<T>()) * x)
        + T(0.14128) * std::cos((T(4.0) * Pi<T>()) * x)
        - T(0.01174) * std::cos((T(6.0) * Pi<T>()) * x);                // original coefficient is 0.01168, modified to ensure 0 at borders
}


//
// FastBlackmanHarrisFilter2 class implementation.
//

template <typename T>
inline FastBlackmanHarrisFilter2<T>::FastBlackmanHarrisFilter2(const T xradius, const T yradius)
  : Filter2<T>(xradius, yradius)
{
}

template <typename T>
inline T FastBlackmanHarrisFilter2<T>::evaluate(const T x, const T y) const
{
    const T nx = T(0.5) * (T(1.0) + x * Filter2<T>::m_rcp_xradius);
    const T ny = T(0.5) * (T(1.0) + y * Filter2<T>::m_rcp_yradius);
    return blackman(nx) * blackman(ny);
}

template <typename T>
APPLESEED_FORCE_INLINE T FastBlackmanHarrisFilter2<T>::blackman(const T x)
{
    return
          T(0.35875)
        - T(0.48829) * fast_cos_full_positive((T(2.0) * Pi<T>()) * x)
        + T(0.14128) * fast_cos_full_positive((T(4.0) * Pi<T>()) * x)
        - T(0.01174) * fast_cos_full_positive((T(6.0) * Pi<T>()) * x);  // original coefficient is 0.01168, modified to ensure 0 at borders
}


//
// Utilities implementation.
//

template <typename Filter>
typename Filter::ValueType compute_normalization_factor(
    const Filter&   filter,
    const size_t    sample_count)
{
    typedef typename Filter::ValueType ValueType;

    const ValueType xradius = filter.get_xradius();
    const ValueType yradius = filter.get_yradius();

    ValueType result(0.0);

    for (size_t i = 0; i < sample_count; ++i)
    {
        static const size_t Bases[1] = { 2 };

        const Vector<ValueType, 2> s =
            hammersley_sequence<ValueType, 2>(Bases, sample_count, i);

        const Vector<ValueType, 2> p(
            xradius * (ValueType(2.0) * s.x - ValueType(1.0)),
            yradius * (ValueType(2.0) * s.y - ValueType(1.0)));

        result += filter.evaluate(p.x, p.y);
    }

    result *= ValueType(4.0) * xradius * yradius;
    result /= static_cast<ValueType>(sample_count);

    return result;
}

}   // namespace foundation
