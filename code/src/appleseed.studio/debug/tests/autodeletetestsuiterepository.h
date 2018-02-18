
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

#ifndef APPLESEED_STUDIO_DEBUG_TESTS_AUTODELETETESTSUITEREPOSITORY_H
#define APPLESEED_STUDIO_DEBUG_TESTS_AUTODELETETESTSUITEREPOSITORY_H

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

namespace appleseed {
namespace studio {

//
// A test suite repository that owns the test suites it contains.
//

class AutoDeleteTestSuiteRepository
  : public foundation::TestSuiteRepository
{
  public:
    // Destructor.
    ~AutoDeleteTestSuiteRepository() override
    {
        clear();
    }

    // Delete and remove all test suites.
    void clear()
    {
        for (size_t i = 0; i < get_suite_count(); ++i)
            delete get_suite(i);

        foundation::TestSuiteRepository::clear();
    }
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_DEBUG_TESTS_AUTODELETETESTSUITEREPOSITORY_H
