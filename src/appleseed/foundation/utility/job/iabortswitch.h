
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

#ifndef APPLESEED_FOUNDATION_UTILITY_JOB_IABORTSWITCH_H
#define APPLESEED_FOUNDATION_UTILITY_JOB_IABORTSWITCH_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// Interface for a thread-safe abort switch.
//

class APPLESEED_DLLSYMBOL IAbortSwitch
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~IAbortSwitch() {}

    // Check whether the abort flag is set.
    virtual bool is_aborted() const = 0;
};


//
// An utility method to check if a nullable abort switch was triggered.
//

inline bool is_aborted(IAbortSwitch* abort_switch)
{
    return abort_switch && abort_switch->is_aborted();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_JOB_IABORTSWITCH_H
