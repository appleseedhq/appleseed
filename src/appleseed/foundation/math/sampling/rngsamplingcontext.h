
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// A sampling context implementing random sampling.
//

template <typename RNG>
class RNGSamplingContext
{
  public:
    // Random number generator type.
    typedef RNG RNGType;

    // Construct a sampling context of dimension 0. It cannot be used
    // directly; only child contexts obtained by splitting can.
    explicit RNGSamplingContext(RNG& rng);

    // Construct a sampling context for a given number of dimensions
    // and samples. Set sample_count to 0 if the required number of
    // samples is unknown or infinite.
    RNGSamplingContext(
        RNG&            rng,
        const size_t    dimension,
        const size_t    sample_count,
        const size_t    initial_instance = 0);

    // Assignment operator.
    RNGSamplingContext& operator=(const RNGSamplingContext& rhs);

    // Trajectory splitting: return a child sampling context for
    // a given number of dimensions and samples.
    RNGSamplingContext split(
        const size_t    dimension,
        const size_t    sample_count) const;

    // In-place trajectory splitting.
    void split_in_place(
        const size_t    dimension,
        const size_t    sample_count);

    // Return the next sample in [0,1)^N.
    // Works for scalars and foundation::Vector<>.
    template <typename T> T next2();

    // Return the total dimension of this sampler.
    size_t get_total_dimension() const;

    // Return the total instance number of this sampler.
    size_t get_total_instance() const;

  private:
    RNG& m_rng;

    template <typename T> struct Tag {};

    template <typename T> T next2(Tag<T>);
    template <typename T, size_t N> Vector<T, N> next2(Tag<Vector<T, N>>);
};


//
// RNGSamplingContext class implementation.
//

template <typename RNG>
inline RNGSamplingContext<RNG>::RNGSamplingContext(RNG& rng)
  : m_rng(rng)
{
}

template <typename RNG>
inline RNGSamplingContext<RNG>::RNGSamplingContext(
    RNG&            rng,
    const size_t    dimension,
    const size_t    sample_count,
    const size_t    initial_instance)
  : m_rng(rng)
{
}

template <typename RNG> inline
RNGSamplingContext<RNG>&
RNGSamplingContext<RNG>::operator=(const RNGSamplingContext& rhs)
{
    return *this;
}

template <typename RNG>
inline RNGSamplingContext<RNG> RNGSamplingContext<RNG>::split(
    const size_t    dimension,
    const size_t    sample_count) const
{
    return
        RNGSamplingContext(
            m_rng,
            dimension,
            sample_count);
}

template <typename RNG>
inline void RNGSamplingContext<RNG>::split_in_place(
    const size_t    dimension,
    const size_t    sample_count)
{
}

template <typename RNG>
template <typename T>
inline T RNGSamplingContext<RNG>::next2()
{
    return next2(Tag<T>());
}

template <typename RNG>
inline size_t RNGSamplingContext<RNG>::get_total_dimension() const
{
    return 0;
}

template <typename RNG>
inline size_t RNGSamplingContext<RNG>::get_total_instance() const
{
    return 0;
}

template <typename RNG>
template <typename T>
inline T RNGSamplingContext<RNG>::next2(Tag<T>)
{
    return rand_double2(m_rng);
}

template <typename RNG>
template <typename T, size_t N>
inline Vector<T, N> RNGSamplingContext<RNG>::next2(Tag<Vector<T, N>>)
{
    Vector<T, N> v;

    for (size_t i = 0; i < N; ++i)
        v[i] = rand_double2(m_rng);

    return v;
}

}   // namespace foundation
