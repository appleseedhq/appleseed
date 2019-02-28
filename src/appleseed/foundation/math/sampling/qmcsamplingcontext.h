
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
#include "foundation/math/permutation.h"
#include "foundation/math/primes.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test/helpers.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Unit test case declarations.
DECLARE_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, InitialStateIsCorrect);
DECLARE_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestCopyConstructor);
DECLARE_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestAssignmentOperator);
DECLARE_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestSplitting);
DECLARE_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestDoubleSplitting);

namespace foundation
{

//
// A sampling context featuring:
//
//   - deterministic sampling based on Halton sequences
//   - Faure digit scrambling
//   - Cranley-Patterson rotation
//   - Monte Carlo padding
//
// Reference:
//
//   Kollig and Keller, Efficient Multidimensional Sampling
//   www.uni-kl.de/AG-Heinrich/EMS.pdf
//

template <typename RNG>
class QMCSamplingContext
{
  public:
    // Random number generator type.
    typedef RNG RNGType;

    // This sampler can operate in two modes:
    //   1. In QMC mode, it uses possibly patent-encumbered techniques.
    //   2. In RNG mode, it works like `RNGSamplingContext` and sticks to random sampling.
    enum Mode { QMCMode, RNGMode };

    // Construct a sampling context of dimension 0.
    // The resulting sampling context cannot be used directly;
    // only child contexts obtained by splitting can.
    QMCSamplingContext(
        RNG&            rng,
        const Mode      mode,
        const size_t    base_instance = 0);

    // Construct a sampling context for a given number of dimensions
    // and samples. Set `sample_count` to 0 if the required number of
    // samples is unknown or infinite.
    QMCSamplingContext(
        RNG&            rng,
        const Mode      mode,
        const size_t    dimension,
        const size_t    sample_count,
        const size_t    instance = 0);

    // Assignment operator.
    // Both sampling contexts must use the same RNG.
    QMCSamplingContext& operator=(const QMCSamplingContext& rhs);

    // Trajectory splitting: return a child sampling context for
    // a given number of dimensions and samples.
    QMCSamplingContext split(
        const size_t    dimension,
        const size_t    sample_count) const;

    // In-place trajectory splitting.
    void split_in_place(
        const size_t    dimension,
        const size_t    sample_count);

    // Set the instance number.
    void set_instance(const size_t instance);

    // Return the next sample in [0,1)^N.
    // Works for scalars and `foundation::Vector<>`.
    template <typename T> T next2();

    // Return the total dimension of this sampler.
    size_t get_total_dimension() const;

    // Return the total instance number of this sampler.
    size_t get_total_instance() const;

  private:
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, InitialStateIsCorrect);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestCopyConstructor);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestAssignmentOperator);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestSplitting);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Sampling_QMCSamplingContext, TestDoubleSplitting);

    typedef Vector<double, 4> VectorType;

    RNG&        m_rng;
    Mode        m_mode;

    size_t      m_base_dimension;
    size_t      m_base_instance;

    size_t      m_dimension;
    size_t      m_sample_count;

    size_t      m_instance;
    VectorType  m_offset;

    // Cranley-Patterson rotation.
    template <typename T>
    static T rotate(T x, const T offset);

    QMCSamplingContext(
        RNG&            rng,
        const Mode      mode,
        const size_t    base_dimension,
        const size_t    base_instance,
        const size_t    dimension,
        const size_t    sample_count);

    void compute_offset();

    template <typename T> struct Tag {};

    template <typename T> T next2(Tag<T>);
    template <typename T, size_t N> Vector<T, N> next2(Tag<Vector<T, N>>);
};


//
// QMCSamplingContext class implementation.
//

template <typename RNG>
inline QMCSamplingContext<RNG>::QMCSamplingContext(
    RNG&                rng,
    const Mode          mode,
    const size_t        base_instance)
  : m_rng(rng)
  , m_mode(mode)
  , m_base_dimension(0)
  , m_base_instance(base_instance)
  , m_dimension(0)
  , m_sample_count(0)
  , m_instance(0)
  , m_offset(0.0)
{
}

template <typename RNG>
inline QMCSamplingContext<RNG>::QMCSamplingContext(
    RNG&                rng,
    const Mode          mode,
    const size_t        dimension,
    const size_t        sample_count,
    const size_t        instance)
  : m_rng(rng)
  , m_mode(mode)
  , m_base_dimension(0)
  , m_base_instance(0)
  , m_dimension(dimension)
  , m_sample_count(sample_count)
  , m_instance(instance)
  , m_offset(0.0)
{
    assert(dimension <= VectorType::Dimension);
}

template <typename RNG>
inline QMCSamplingContext<RNG>::QMCSamplingContext(
    RNG&                rng,
    const Mode          mode,
    const size_t        base_dimension,
    const size_t        base_instance,
    const size_t        dimension,
    const size_t        sample_count)
  : m_rng(rng)
  , m_mode(mode)
  , m_base_dimension(base_dimension)
  , m_base_instance(base_instance)
  , m_dimension(dimension)
  , m_sample_count(sample_count)
  , m_instance(0)
{
    assert(dimension <= VectorType::Dimension);

    if (m_mode == QMCMode)
        compute_offset();
}

template <typename RNG> inline
QMCSamplingContext<RNG>&
QMCSamplingContext<RNG>::operator=(const QMCSamplingContext& rhs)
{
    assert(&m_rng == &rhs.m_rng);

    m_mode = rhs.m_mode;
    m_base_dimension = rhs.m_base_dimension;
    m_base_instance = rhs.m_base_instance;
    m_dimension = rhs.m_dimension;
    m_sample_count = rhs.m_sample_count;
    m_instance = rhs.m_instance;
    m_offset = rhs.m_offset;

    return *this;
}

template <typename RNG>
inline QMCSamplingContext<RNG> QMCSamplingContext<RNG>::split(
    const size_t        dimension,
    const size_t        sample_count) const
{
    return
        QMCSamplingContext(
            m_rng,
            m_mode,
            m_base_dimension + m_dimension,         // dimension allocation
            m_base_instance + m_instance,           // decorrelation by generalization
            dimension,
            sample_count);
}

template <typename RNG>
inline void QMCSamplingContext<RNG>::split_in_place(
    const size_t        dimension,
    const size_t        sample_count)
{
    assert(m_sample_count == 0 || m_instance == m_sample_count);    // can't split in the middle of a sequence
    assert(dimension <= VectorType::Dimension);

    m_base_dimension += m_dimension;                // dimension allocation
    m_base_instance += m_instance;                  // decorrelation by generalization
    m_dimension = dimension;
    m_sample_count = sample_count;
    m_instance = 0;

    if (m_mode == QMCMode)
        compute_offset();
}

template <typename RNG>
inline void QMCSamplingContext<RNG>::set_instance(const size_t instance)
{
    m_instance = instance;
}

template <typename RNG>
template <typename T>
inline T QMCSamplingContext<RNG>::next2()
{
    return next2(Tag<T>());
}

template <typename RNG>
inline size_t QMCSamplingContext<RNG>::get_total_dimension() const
{
    return m_base_dimension + m_dimension;
}

template <typename RNG>
inline size_t QMCSamplingContext<RNG>::get_total_instance() const
{
    return m_base_instance + m_instance;
}

template <typename RNG>
template <typename T>
inline T QMCSamplingContext<RNG>::rotate(T x, const T offset)
{
    assert(offset >= T(0.0));

    x += offset;

    if (x >= T(1.0))
        x -= T(1.0);

    return x;
}

template <typename RNG>
inline void QMCSamplingContext<RNG>::compute_offset()
{
    for (size_t i = 0, d = m_base_dimension; i < m_dimension; ++i, ++d)
    {
        if (d < FaurePermutationTableSize)
        {
            assert(d < PrimeTableSize);

            m_offset[i] =
                fast_permuted_radical_inverse<double>(
                    d,
                    FaurePermutations[d],
                    m_base_instance);
        }
        else
        {
            // Monte Carlo padding.
            m_offset[i] = rand_double2(m_rng);
        }
    }
}

template <typename RNG>
template <typename T>
inline T QMCSamplingContext<RNG>::next2(Tag<T>)
{
    return next2(Tag<Vector<T, 1>>())[0];
}

template <typename RNG>
template <typename T, size_t N>
inline Vector<T, N> QMCSamplingContext<RNG>::next2(Tag<Vector<T, N>>)
{
    Vector<T, N> v;

    assert(m_sample_count == 0 || m_instance < m_sample_count);
    assert(N == m_dimension);
    assert(N <= PrimeTableSize);

    if (m_mode == QMCMode)
    {
        if (m_instance < PrecomputedHaltonSequenceSize)
        {
            for (size_t i = 0; i < N; ++i)
            {
                v[i] = static_cast<T>(PrecomputedHaltonSequence[m_instance * 4 + i]);
                v[i] = rotate(v[i], static_cast<T>(m_offset[i]));
            }
        }
        else
        {
            v[0] = radical_inverse_base2<T>(m_instance);
            v[0] = rotate(v[0], static_cast<T>(m_offset[0]));

            for (size_t i = 1; i < N; ++i)
            {
                v[i] = fast_radical_inverse<T>(i, m_instance);
                v[i] = rotate(v[i], static_cast<T>(m_offset[i]));
            }
        }
    }
    else
    {
        for (size_t i = 0; i < N; ++i)
            v[i] = rand2<T>(m_rng);
    }

    ++m_instance;

    return v;
}

}   // namespace foundation
