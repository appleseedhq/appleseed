
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_SEEXPR_H
#define APPLESEED_FOUNDATION_UTILITY_SEEXPR_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Boost headers.
#include "boost/regex.hpp"

// Standard headers.
#include <map>
#include <string>
#include <vector>

namespace foundation
{

//
// Extracts all file paths from a valid SeExpr expression.
//

class SeExprFilePathExtractor
  : public NonCopyable
{
  public:
    typedef std::vector<std::string> PathCollection;
    typedef std::map<std::string, std::string> MappingCollection;

    SeExprFilePathExtractor();

    void extract_paths(
        const std::string&          expression,
        PathCollection&             paths) const;

    std::string replace_paths(
        const std::string&          expression,
        const MappingCollection&    mappings) const;

  private:
    const boost::regex m_regex;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SEEXPR_H
