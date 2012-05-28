
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_UTILITY_TRANSFORMSEQUENCE_H
#define APPLESEED_RENDERER_UTILITY_TRANSFORMSEQUENCE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/transform.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Versionable; }

namespace renderer
{

//
// A piecewise linear rigid transform path as a function of time.
//

class DLLSYMBOL TransformSequence
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit TransformSequence(foundation::Versionable* parent = 0);

    // Destructor.
    ~TransformSequence();

    // Remove all transforms from the sequence.
    void clear();

    // Set a transform at a given time.
    // Replaces any transform already set at this time.
    void set_transform(
        const double                    time,
        const foundation::Transformd&   transform);

    // Return a given (time, transform) pair.
    void get_transform(
        const size_t                    index,
        double&                         time,
        foundation::Transformd&         transform) const;

    // Access the transform at the lowest time value.
    foundation::Transformd& earliest_transform();
    const foundation::Transformd& earliest_transform() const;

    // Return true if the sequence is empty.
    bool empty() const;

    // Return the number of transforms in the sequence.
    size_t size() const;

    // Prepare the sequence for quick evaluation.
    // This method must be called after new transforms have been set and
    // before any call to evaluate(). It may be called multiple times.
    void prepare();

    // Compute the transform at any given time.
    foundation::Transformd evaluate(const double time) const;

  private:
    struct TransformKey
    {
        double                          m_time;
        foundation::Transformd          m_transform;

        bool operator<(const TransformKey& rhs) const
        {
            return m_time < rhs.m_time;
        }
    };

    foundation::Versionable*            m_parent;
    size_t                              m_capacity;
    size_t                              m_size;
    TransformKey*                       m_keys;
    foundation::TransformInterpolatord* m_interpolators;
};


//
// TransformSequence class implementation.
//

inline bool TransformSequence::empty() const
{
    return m_size == 0;
}

inline size_t TransformSequence::size() const
{
    return m_size;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_TRANSFORMSEQUENCE_H
