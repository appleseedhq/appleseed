
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_BENCHMARK_BENCHMARKDATAPOINT_H
#define APPLESEED_FOUNDATION_UTILITY_BENCHMARK_BENCHMARKDATAPOINT_H

// appleseed.foundation headers.
#include "foundation/platform/datetime.h"
#include "foundation/platform/types.h"

// boost headers.
#include "boost/date_time/gregorian/gregorian.hpp"
#include "boost/date_time/posix_time/posix_time.hpp"

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

class FOUNDATIONDLL BenchmarkDataPoint
{
  public:
    BenchmarkDataPoint();

    BenchmarkDataPoint(
        const boost::posix_time::ptime& date,
        const double                    ticks);

    boost::posix_time::ptime get_date() const;

    double get_ticks() const;

  private:
    uint64  m_date_microseconds;
    double  m_ticks;
};


//
// Implementation.
//

namespace benchmark_impl
{
    const boost::posix_time::ptime Epoch(boost::gregorian::date(1970, 1, 1));

    inline uint64 ptime_to_microseconds(const boost::posix_time::ptime& time)
    {
        return (time - Epoch).total_microseconds();
    }

    inline boost::posix_time::ptime microseconds_to_ptime(const uint64 microseconds)
    {
        return Epoch + microseconds_to_time_duration(microseconds);
    }
}

inline BenchmarkDataPoint::BenchmarkDataPoint()
{
}

inline BenchmarkDataPoint::BenchmarkDataPoint(
    const boost::posix_time::ptime&     date,
    const double                        ticks)
  : m_date_microseconds(benchmark_impl::ptime_to_microseconds(date))
  , m_ticks(ticks)
{
}

inline boost::posix_time::ptime BenchmarkDataPoint::get_date() const
{
    return benchmark_impl::microseconds_to_ptime(m_date_microseconds);
}

inline double BenchmarkDataPoint::get_ticks() const
{
    return m_ticks;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_BENCHMARK_BENCHMARKDATAPOINT_H
