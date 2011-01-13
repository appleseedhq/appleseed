
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_POPULATION_H
#define APPLESEED_FOUNDATION_MATH_POPULATION_H

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

    // Insert a new value into the population.
    void insert(const ValueType& val);

    // Return the size of the population.
    size_t get_size() const;

    // Return various properties of the population defined so far.
    ValueType get_min() const;              // minimum value
    ValueType get_max() const;              // maximum value
    double get_avg() const;                 // average value
    double get_dev() const;                 // standard deviation

  private:
    size_t      m_size;                     // size of the population
    ValueType   m_min;                      // minimum value
    ValueType   m_max;                      // maximum value
    double      m_avg;                      // average value
    double      m_s;                        // s = n*(standard deviation)^2
};


//
// Population class implementation.
//

// Constructor.
template <typename T>
inline Population<T>::Population()
  : m_size(0)
  , m_min(std::numeric_limits<ValueType>::max())
  , m_max(std::numeric_limits<ValueType>::min())
  , m_avg(0.0)
  , m_s(0.0)
{
}

// Insert a new value into the population.
template <typename T>
inline void Population<T>::insert(const ValueType& val)
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
    //   mean_(n+1) = mean_n + delta
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

    // Update the minimum value.
    if (m_min > val)
        m_min = val;

    // Update the maximum value.
    if (m_max < val)
        m_max = val;

    // Compute residual value.
    const double residual = double(val) - m_avg;

    // Compute the new size of the population.
    const size_t new_size = m_size + 1;

    // Compute the new mean value of the population.
    const double new_avg = m_avg + residual / new_size;

    // Update s.
    m_s += residual * (double(val) - new_avg);

    // Update the mean value of the population.
    m_avg = new_avg;

    // Update the size of the population.
    m_size = new_size;
}

// Return the size of the population.
template <typename T>
inline size_t Population<T>::get_size() const
{
    return m_size;
}

// Return various properties of the population defined so far.
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
inline double Population<T>::get_avg() const
{
    return m_avg;
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_POPULATION_H
