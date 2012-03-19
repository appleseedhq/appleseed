
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

// Interface header.
#include "statistics.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <iomanip>
#include <sstream>

using namespace std;

namespace foundation
{

Statistics::Statistics(const string& title)
  : m_title(title)
{
}

namespace
{
    template <typename T>
    string pop_to_string(const Population<T>& pop)
    {
        stringstream sstr;
        sstr << fixed << setprecision(1);

        sstr <<   "avg " << pop.get_avg();
        sstr << "  min " << pop.get_min();
        sstr << "  max " << pop.get_max();
        sstr << "  dev " << pop.get_dev();

        return sstr.str();
    }
}

string Statistics::to_string(const size_t max_title_length) const
{
    stringstream sstr;
    sstr << m_title << ":" << endl;

    if (m_records.empty())
        sstr << "  no statistics" << endl;
    else
    {
        for (const_each<RecordVector> i = m_records; i; ++i)
        {
            sstr << "  ";

            if (i->m_title.size() > max_title_length)
                sstr << i->m_title.substr(0, max_title_length);
            else sstr << i->m_title << string(max_title_length - i->m_title.size(), ' ');

            sstr << ' ';

            switch (i->m_type)
            {
              case Record::UnsignedInteger:
                sstr << pretty_uint(m_uint_values[i->m_index]);
                break;

              case Record::UnsignedIntegerPopulation:
                sstr << pop_to_string(m_uint_pop_values[i->m_index]);
                break;

              case Record::FloatingPoint:
                sstr << pretty_scalar(m_fp_values[i->m_index]);
                break;

              case Record::FloatingPointPopulation:
                sstr << pop_to_string(m_fp_pop_values[i->m_index]);
                break;

              case Record::String:
                sstr << m_string_values[i->m_index];
                break;
              assert_otherwise;
            }

            sstr << endl;
        }
    }

    return sstr.str();
}

}   // namespace foundation
