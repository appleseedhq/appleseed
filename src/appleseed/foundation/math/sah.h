
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/memory/memory.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <vector>

namespace foundation
{

//
// Surface Area Heuristic (SAH) function, exact version.
//

template <typename T>
class ExactSAHFunction
  : public NonCopyable
{
  public:
    typedef T ValueType;
    typedef AABB<T, 3> AABBType;

    // Constructor.
    ExactSAHFunction();

    // Reset the function and set the bounds of the domain.
    // Both bounds of the domain are exclusive: minimize()
    // will never report them as valid abscissas. This method
    // must be called once before intervals can be inserted.
    void reset(
        const ValueType domain_begin,
        const ValueType domain_end);

    // Insert an interval into the function.
    void insert(
        const ValueType interval_begin,
        const ValueType interval_end);

    // Find the abscissa with the smallest cost, with respect
    // to the surface area heuristic.
    void minimize(
        const AABBType& bbox,
        const size_t    dimension,
        ValueType&      min_cost,
        ValueType&      min_abscissa);

    // Visit the SAH function, useful for debugging.
    template <typename Visitor>
    void visit(
        const AABBType& bbox,
        const size_t    dimension,
        Visitor&        visitor);

  private:
    // Event.
    struct Event
    {
        // Event type.
        enum Type { Entry, Exit };

        ValueType       m_abscissa;
        Type            m_type;

        // Constructor.
        Event(
            const ValueType abscissa,
            const Type      type)
          : m_abscissa(abscissa)
          , m_type(type)
        {
        }

        // Define an order on events.
        bool operator<(const Event& rhs) const
        {
            // Sort by increasing abscissa.
            if (m_abscissa < rhs.m_abscissa)
                return true;
            else if (m_abscissa > rhs.m_abscissa)
                return false;
            else
            {
                // When two events have the same abscissa,
                // put Entry events before Exit events.
                return m_type == Entry && rhs.m_type == Exit;
            }
        }
    };

    typedef std::vector<Event> EventVector;

#ifndef NDEBUG
    bool                m_was_reset;
#endif

    ValueType           m_domain_begin;
    ValueType           m_domain_end;
    EventVector         m_events;
    size_t              m_initial_left_count;
    size_t              m_interval_count;
};


//
// Surface Area Heuristic (SAH) function, approximate version.
//

template <typename T, size_t BinCount>
class ApproxSAHFunction
  : public NonCopyable
{
  public:
    typedef T ValueType;
    typedef AABB<T, 3> AABBType;

    // Constructor. Both bounds of the domain are exclusive:
    // minimize() will never report them as valid abscissas.
    ApproxSAHFunction(
        const ValueType domain_begin,
        const ValueType domain_end);

    // Insert an interval into the function.
    void insert(
        const ValueType interval_begin,
        const ValueType interval_end);

    // Find the abscissa with the smallest cost, with respect
    // to the surface area heuristic.
    void minimize(
        const AABBType& bbox,
        const size_t    dimension,
        ValueType&      min_cost,
        ValueType&      min_abscissa);

    // Visit the SAH function, useful for debugging.
    template <typename Visitor>
    void visit(
        const AABBType& bbox,
        const size_t    dimension,
        Visitor&        visitor) const;

  private:
    // Event.
    struct Event
    {
        size_t  m_entry;
        size_t  m_exit;
    };

    const ValueType     m_domain_begin;
    const ValueType     m_domain_end;
    const ValueType     m_domain_length;
    const ValueType     m_domain_scale;
    Event               m_events[BinCount];
    size_t              m_initial_left_count;
    size_t              m_interval_count;
};


//
// Surface area heuristic cost function.
//

template <typename T>
class SAHCostFunction
{
  public:
    typedef T ValueType;
    typedef AABB<T, 3> AABBType;

    SAHCostFunction(
        const AABBType& bbox,
        const size_t    dimension)
    {
        // Fast modulo 3.
        // See http://www.codercorner.com/Modulo3.htm.
        const size_t d0 = dimension;
        const size_t d1 = (1UL << d0) & 3;
        const size_t d2 = (1UL << d1) & 3;

        // Compute the extent of the parent bounding box.
        const ValueType e1 = bbox.max[d1] - bbox.min[d1];
        const ValueType e2 = bbox.max[d2] - bbox.min[d2];

        // Compute the terms of the surface area expressions.
        m_a = e1 + e2;
        m_b = e1 * e2;
    }

    ValueType operator()(
        const ValueType left_length,
        const ValueType right_length,
        const size_t    left_count,
        const size_t    right_count) const
    {
        // Compute the probabilities of hitting the child bounding boxes.
        const ValueType left_prob = left_length * m_a + m_b;
        const ValueType right_prob = right_length * m_a + m_b;

        // Compute and return the cost of this split according to the SAH.
        return left_prob * left_count + right_prob * right_count;
    }

  private:
    ValueType m_a, m_b;
};


//
// ExactSAHFunction class implementation.
//

template <typename T>
ExactSAHFunction<T>::ExactSAHFunction()
#ifndef NDEBUG
  : m_was_reset(false)
#endif
{
}

template <typename T>
inline void ExactSAHFunction<T>::reset(
    const ValueType domain_begin,
    const ValueType domain_end)
{
    assert(domain_begin < domain_end);
    m_domain_begin = domain_begin;
    m_domain_end = domain_end;

    clear_keep_memory(m_events);

    m_initial_left_count = 0;
    m_interval_count = 0;

#ifndef NDEBUG
    m_was_reset = true;
#endif
}

template <typename T>
inline void ExactSAHFunction<T>::insert(
    const ValueType interval_begin,
    const ValueType interval_end)
{
    assert(m_was_reset);
    assert(interval_begin <= interval_end);

    // Reject intervals entirely outside the bounds of the domain.
    if (interval_begin >= m_domain_end || interval_end <= m_domain_begin)
        return;

    // Create Entry event.
    if (interval_begin > m_domain_begin)
        m_events.push_back(Event(interval_begin, Event::Entry));
    else ++m_initial_left_count;

    // Create Exit event.
    if (interval_end < m_domain_end)
        m_events.push_back(Event(interval_end, Event::Exit));

    ++m_interval_count;
}

template <typename T>
void ExactSAHFunction<T>::minimize(
    const AABBType& bbox,
    const size_t    dimension,
    ValueType&      min_cost,
    ValueType&      min_abscissa)
{
    assert(m_was_reset);

    min_cost = std::numeric_limits<ValueType>::max();

    // Sort events by increasing abscissa.
    std::sort(m_events.begin(), m_events.end());

    SAHCostFunction<T> cost_function(bbox, dimension);

    size_t left_count = m_initial_left_count;
    size_t right_count = m_interval_count;

    // Walk the list of events, keeping track of the abscissa with the lowest cost.
    for (const_each<EventVector> i = m_events; i; ++i)
    {
        // Compute the left and right lengths.
        const ValueType left_length = i->m_abscissa - m_domain_begin;
        const ValueType right_length = m_domain_end - i->m_abscissa;

        assert(left_length > ValueType(0.0));
        assert(right_length > ValueType(0.0));

        // Update the item counters.
        if (i->m_type == Event::Entry)
            ++left_count;
        else --right_count;

        // Evaluate the cost function at this abscissa.
        const ValueType cost =
            cost_function(
                left_length,
                right_length,
                left_count,
                right_count);

        // Keep track of the abscissa with the lowest cost.
        if (min_cost > cost)
        {
            min_cost = cost;
            min_abscissa = i->m_abscissa;
        }
    }
}

template <typename T>
template <typename Visitor>
void ExactSAHFunction<T>::visit(
    const AABBType& bbox,
    const size_t    dimension,
    Visitor&        visitor)
{
    assert(m_was_reset);

    // Sort events by increasing abscissa.
    std::sort(m_events.begin(), m_events.end());

    SAHCostFunction<T> cost_function(bbox, dimension);

    size_t left_count = m_initial_left_count;
    size_t right_count = m_interval_count;

    // Walk the list of events, keeping track of the abscissa with the lowest cost.
    for (const_each<EventVector> i = m_events; i; ++i)
    {
        // Compute the left and right lengths.
        const ValueType left_length = i->m_abscissa - m_domain_begin;
        const ValueType right_length = m_domain_end - i->m_abscissa;

        assert(left_length > ValueType(0.0));
        assert(left_length < m_domain_end - m_domain_begin);
        assert(right_length > ValueType(0.0));
        assert(right_length < m_domain_end - m_domain_begin);

        // Update the item counters.
        if (i->m_type == Event::Entry)
            ++left_count;
        else --right_count;

        // Evaluate the cost function at this abscissa.
        const ValueType cost =
            cost_function(
                left_length,
                right_length,
                left_count,
                right_count);

        // Invoke the visitor for this point.
        visitor.visit(
            m_domain_begin,
            m_domain_end,
            left_length,
            right_length,
            left_count,
            right_count,
            cost);
    }
}


//
// ApproxSAHFunction class implementation.
//

template <typename T, size_t BinCount>
ApproxSAHFunction<T, BinCount>::ApproxSAHFunction(
    const ValueType domain_begin,
    const ValueType domain_end)
  : m_domain_begin(domain_begin)
  , m_domain_end(domain_end)
  , m_domain_length(m_domain_end - m_domain_begin)
  , m_domain_scale(static_cast<ValueType>(BinCount + 1) / m_domain_length)
  , m_initial_left_count(0)
  , m_interval_count(0)
{
    assert(m_domain_begin < m_domain_end);
    for (size_t i = 0; i < BinCount; ++i)
    {
        m_events[i].m_entry = 0;
        m_events[i].m_exit = 0;
    }
}

template <typename T, size_t BinCount>
inline void ApproxSAHFunction<T, BinCount>::insert(
    const ValueType interval_begin,
    const ValueType interval_end)
{
    assert(interval_begin <= interval_end);

    // Reject intervals entirely outside the bounds of the domain.
    if (interval_begin >= m_domain_end || interval_end <= m_domain_begin)
        return;

    // Create Entry event.
    if (interval_begin > m_domain_begin)
    {
        const ValueType x = interval_begin - m_domain_begin;
        const size_t bin = truncate<size_t>(x * m_domain_scale);
        if (bin < BinCount)
            ++m_events[bin].m_entry;
    }
    else ++m_initial_left_count;

    // Create Exit event.
    if (interval_end < m_domain_end)
    {
        const ValueType x = interval_end - m_domain_begin;
        const size_t bin = truncate<size_t>(x * m_domain_scale);
        if (bin < BinCount)
            ++m_events[bin].m_exit;
    }

    ++m_interval_count;
}

template <typename T, size_t BinCount>
void ApproxSAHFunction<T, BinCount>::minimize(
    const AABBType& bbox,
    const size_t    dimension,
    ValueType&      min_cost,
    ValueType&      min_abscissa)
{
    ValueType min_left_length = ValueType(0.0);
    min_cost = std::numeric_limits<ValueType>::max();

    SAHCostFunction<T> cost_function(bbox, dimension);

    size_t left_count = m_initial_left_count;
    size_t right_count = m_interval_count;

    const ValueType scale = m_domain_length / (BinCount + 1);

    for (size_t i = 0; i < BinCount; ++i)
    {
        // Compute the left and right lengths.
        const ValueType left_length = static_cast<ValueType>(i + 1) * scale;
        const ValueType right_length = m_domain_length - left_length;

        assert(left_length > ValueType(0.0));
        assert(left_length < m_domain_length);
        assert(right_length > ValueType(0.0));
        assert(right_length < m_domain_length);

        // Update the item counters.
        left_count += m_events[i].m_entry;
        right_count -= m_events[i].m_exit;

        // Evaluate the cost function at this abscissa.
        const ValueType cost =
            cost_function(
                left_length,
                right_length,
                left_count,
                right_count);

        // Keep track of the abscissa with the lowest cost.
        if (min_cost > cost)
        {
            min_cost = cost;
            min_left_length = left_length;
        }
    }

    // Compute the actual abscissa with the lowest cost.
    assert(min_cost < std::numeric_limits<ValueType>::max());
    min_abscissa = m_domain_begin + min_left_length;
}

template <typename T, size_t BinCount>
template <typename Visitor>
void ApproxSAHFunction<T, BinCount>::visit(
    const AABBType& bbox,
    const size_t    dimension,
    Visitor&        visitor) const
{
    SAHCostFunction<T> cost_function(bbox, dimension);

    size_t left_count = m_initial_left_count;
    size_t right_count = m_interval_count;

    const ValueType scale = m_domain_length / (BinCount + 1);

    for (size_t i = 0; i < BinCount; ++i)
    {
        // Compute the left and right lengths.
        const ValueType left_length = static_cast<ValueType>(i + 1) * scale;
        const ValueType right_length = m_domain_length - left_length;

        assert(left_length > ValueType(0.0));
        assert(left_length < m_domain_length);
        assert(right_length > ValueType(0.0));
        assert(right_length < m_domain_length);

        // Update the item counters.
        left_count += m_events[i].m_entry;
        right_count -= m_events[i].m_exit;

        // Evaluate the cost function at this abscissa.
        const ValueType cost =
            cost_function(
                left_length,
                right_length,
                left_count,
                right_count);

        // Invoke the visitor for this point.
        visitor.visit(
            m_domain_begin,
            m_domain_end,
            left_length,
            right_length,
            left_count,
            right_count,
            cost);
    }
}

}   // namespace foundation
