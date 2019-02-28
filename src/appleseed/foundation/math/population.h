
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
#include "foundation/math/scalar.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <limits>

namespace foundation
{

//
// Compute statistical properties of a population of values.
//

template <typename T>
class Population
{
  public:
    // Value and population type.
    typedef T ValueType;
    typedef Population<T> PopulationType;

    // Constructor.
    Population();                           // empty population

    // Insert `count` times the value `val` into the population.
    void insert(const ValueType& val, const size_t count = 1);

    // Merge another population into this one.
    void merge(const PopulationType& pop);

    // Return the size of the population.
    size_t get_size() const;

    // Return various properties of the population defined so far.
    ValueType get_min() const;              // minimum value
    ValueType get_max() const;              // maximum value
    double get_mean() const;                // mean value
    double get_dev() const;                 // standard deviation
    double get_var() const;                 // coefficient of variation

  private:
    size_t      m_size;                     // size of the population
    ValueType   m_min;                      // minimum value
    ValueType   m_max;                      // maximum value
    double      m_mean;                     // mean value
    double      m_s;                        // s = n*(standard deviation)^2
};


//
// Population class implementation.
//

template <typename T>
inline Population<T>::Population()
  : m_size(0)
  , m_min(std::numeric_limits<ValueType>::max())
  , m_max(signed_min<ValueType>())
  , m_mean(0.0)
  , m_s(0.0)
{
}

template <typename T>
inline void Population<T>::insert(const ValueType& val, const size_t count)
{
    //
    // For a given population of n values { x1, x2, ..., xn }, the minimum value,
    // the maximum value, the mean (or average) value and the standard deviation
    // of the population are defined as follow:
    //
    //   minimum value          min { x1, x2, ..., xn }
    //   maximum value          max { x1, x2, ..., xn }
    //   mean value             sum(xi, i=1..n) / n
    //   standard deviation     sqrt( sum((xi-mean)^2, i=1..n) / n )
    //
    // To update the mean value when a new value x is included into the population,
    // we write
    //
    //   mean_{n+1} = mean_n + delta
    //
    // and solve this equation for delta, which yields
    //
    //   delta = (x - mean_n) / (n + 1)
    //
    // The equations and the algorithm to update the standard deviation are
    // detailed in the references.
    //
    // References:
    //
    //   http://en.wikipedia.org/wiki/Standard_deviation
    //   http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    //

    assert(count >= 1);

    // Update the minimum value.
    if (m_min > val)
        m_min = val;

    // Update the maximum value.
    if (m_max < val)
        m_max = val;

    // Update the size of the population.
    m_size += count;

    // Compute the residual value.
    const double double_val = static_cast<double>(val);
    const double residual = (double_val - m_mean) * count;

    // Update the mean value of the population.
    m_mean += residual / m_size;

    // Update s.
    m_s += residual * (double_val - m_mean);
}

template <typename T>
void Population<T>::merge(const PopulationType& pop)
{
    //
    // Reference:
    //
    //   Stack Overflow: Merging two statistical result sets
    //   http://stackoverflow.com/questions/1480626/merging-two-statistical-result-sets
    //

    // Update the minimum value.
    if (m_min > pop.m_min)
        m_min = pop.m_min;

    // Update the maximum value.
    if (m_max < pop.m_max)
        m_max = pop.m_max;

    // Compute the new size of the population.
    const size_t new_size = m_size + pop.m_size;

    // Compute the new mean value of the population.
    const double new_mean =
        new_size > 0
            ? (m_mean * m_size + pop.m_mean * pop.m_size) / new_size
            : 0.0;

    // Update s.
    const double var1 = m_size > 0 ? m_s / m_size : 0.0;
    const double var2 = pop.m_size > 0 ? pop.m_s / pop.m_size : 0.0;
    m_s = (var1 + square(m_mean - new_mean)) * m_size + (var2 + square(pop.m_mean - new_mean)) * pop.m_size;

    // Update the size of the population.
    m_size = new_size;

    // Update the mean value of the population.
    m_mean = new_mean;
}

template <typename T>
inline size_t Population<T>::get_size() const
{
    return m_size;
}

template <typename T>
inline T Population<T>::get_min() const
{
    return m_size > 0 ? m_min : ValueType(0);
}

template <typename T>
inline T Population<T>::get_max() const
{
    return m_size > 0 ? m_max : ValueType(0);
}

template <typename T>
inline double Population<T>::get_mean() const
{
    return m_mean;
}

template <typename T>
inline double Population<T>::get_dev() const
{
    //
    // Since
    //
    //   s = n*(standard deviation)^2
    //
    // the standard deviation is equal to
    //
    //   sigma = sqrt(s/n)
    //

    return m_size > 0 ? std::sqrt(m_s / m_size) : 0.0;
}

template <typename T>
inline double Population<T>::get_var() const
{
    const double dev = get_dev();
    return m_mean == 0.0 ? dev : dev / m_mean;
}

}   // namespace foundation
