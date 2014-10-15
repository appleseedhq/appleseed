
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_JOB_ABORTSWITCH_H
#define APPLESEED_FOUNDATION_UTILITY_JOB_ABORTSWITCH_H

// appleseed.foundation headers.
#include "foundation/platform/thread.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Boost headers.
#include "boost/cstdint.hpp"

namespace foundation
{

//
// A thread-safe switch to instruct threads to abort their computations.
//

class DLLSYMBOL AbortSwitch
{
  public:
    // Constructor, clears the abort flag.
    AbortSwitch();

    // Clear the abort flag.
    void clear();

    // Set the abort flag.
    void abort();

    // Check whether the abort flag is set.
    bool is_aborted() const;

  private:
    mutable volatile boost::uint32_t m_aborted;
};


//
// An utility method to check if a nullable abort switch was triggered.
//

inline bool is_aborted(AbortSwitch* abort_switch)
{
    return abort_switch && abort_switch->is_aborted();
}


//
// AbortSwitch class implementation.
//

inline AbortSwitch::AbortSwitch()
{
    clear();
}

inline void AbortSwitch::clear()
{
    boost_atomic::atomic_write32(&m_aborted, 0);
}

inline void AbortSwitch::abort()
{
    boost_atomic::atomic_write32(&m_aborted, 1);
}

inline bool AbortSwitch::is_aborted() const
{
    return boost_atomic::atomic_read32(&m_aborted) == 1;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_JOB_ABORTSWITCH_H
