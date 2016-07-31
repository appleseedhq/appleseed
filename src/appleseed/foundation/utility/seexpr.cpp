
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "seexpr.h"

using namespace boost;
using namespace std;

namespace foundation
{

//
// SeExprFilePathExtractor class implementation.
//

SeExprFilePathExtractor::SeExprFilePathExtractor()
  : m_regex("(?<prefix>\\btexture\\(\\s*\")(?<path>[^\"]+)(?<suffix>\")")
{
}

void SeExprFilePathExtractor::extract_paths(
    const string&               expression,
    PathCollection&             paths) const
{
    string::const_iterator start = expression.begin();
    string::const_iterator end = expression.end();
    smatch matches;

    while (regex_search(start, end, matches, m_regex))
    {
        sub_match<string::const_iterator> submatch = matches["path"];
        paths.push_back(submatch.str());
        start = submatch.second;
    }
}

namespace
{
    struct Formatter
    {
        const SeExprFilePathExtractor::MappingCollection& m_mappings;

        explicit Formatter(const SeExprFilePathExtractor::MappingCollection& mappings)
          : m_mappings(mappings)
        {
        }

        string operator()(const smatch& matches) const
        {
            string result = matches["prefix"].str();
            result += m_mappings.find(matches["path"].str())->second;
            result += matches["suffix"].str();
            return result;
        }
    };
}

string SeExprFilePathExtractor::replace_paths(
    const string&               expression,
    const MappingCollection&    mappings) const
{
    const Formatter formatter(mappings);
    return regex_replace(expression, m_regex, formatter);
}

}   // namespace foundation
