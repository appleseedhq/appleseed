
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
#include "foundation/platform/datetime.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Boost headers.
#include "boost/date_time/gregorian/gregorian.hpp"
#include "boost/date_time/posix_time/posix_time.hpp"

// Standard headers.
#include <cstdint>

namespace foundation
{

class APPLESEED_DLLSYMBOL BenchmarkDataPoint
{
  public:
    BenchmarkDataPoint();

    BenchmarkDataPoint(
        const boost::posix_time::ptime& date,
        const double                    ticks);

    static std::uint64_t ptime_to_microseconds(const boost::posix_time::ptime& time);

    static boost::posix_time::ptime microseconds_to_ptime(const std::uint64_t microseconds);

    boost::posix_time::ptime get_date() const;

    double get_ticks() const;

    bool operator==(const BenchmarkDataPoint& rhs) const;
    bool operator!=(const BenchmarkDataPoint& rhs) const;
    bool operator<(const BenchmarkDataPoint& rhs) const;

  private:
    std::uint64_t   m_date_microseconds;
    double          m_ticks;
};


//
// Implementation.
//

namespace benchmark_impl
{
    const boost::posix_time::ptime Epoch(boost::gregorian::date(1970, 1, 1));
}

inline BenchmarkDataPoint::BenchmarkDataPoint()
{
}

inline BenchmarkDataPoint::BenchmarkDataPoint(
    const boost::posix_time::ptime&     date,
    const double                        ticks)
  : m_date_microseconds(ptime_to_microseconds(date))
  , m_ticks(ticks)
{
}

inline std::uint64_t BenchmarkDataPoint::ptime_to_microseconds(const boost::posix_time::ptime& time)
{
    return (time - benchmark_impl::Epoch).total_microseconds();
}

inline boost::posix_time::ptime BenchmarkDataPoint::microseconds_to_ptime(const std::uint64_t microseconds)
{
    return benchmark_impl::Epoch + microseconds_to_time_duration(microseconds);
}

inline boost::posix_time::ptime BenchmarkDataPoint::get_date() const
{
    return microseconds_to_ptime(m_date_microseconds);
}

inline double BenchmarkDataPoint::get_ticks() const
{
    return m_ticks;
}

inline bool BenchmarkDataPoint::operator==(const BenchmarkDataPoint& rhs) const
{
    return m_date_microseconds == rhs.m_date_microseconds && m_ticks == rhs.m_ticks;
}

inline bool BenchmarkDataPoint::operator!=(const BenchmarkDataPoint& rhs) const
{
    return !(*this == rhs);
}

inline bool BenchmarkDataPoint::operator<(const BenchmarkDataPoint& rhs) const
{
    return m_date_microseconds < rhs.m_date_microseconds;
}

}   // namespace foundation
