
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/job/iabortswitch.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// A thread-safe switch to instruct threads to abort their computations.
//

class APPLESEED_DLLSYMBOL AbortSwitch
  : public IAbortSwitch
{
  public:
    // Clear the abort flag.
    void clear();

    // Set the abort flag.
    void abort();

    // Check whether the abort flag is set.
    bool is_aborted() const override;

  private:
    ThreadFlag m_abort_flag;
};


//
// AbortSwitch class implementation.
//

inline void AbortSwitch::clear()
{
    m_abort_flag.clear();
}

inline void AbortSwitch::abort()
{
    m_abort_flag.set();
}

inline bool AbortSwitch::is_aborted() const
{
    return m_abort_flag.is_set();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_JOB_ABORTSWITCH_H
