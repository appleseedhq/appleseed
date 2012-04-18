
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

#ifndef APPLESEED_FOUNDATION_UTILITY_STATISTICS_H
#define APPLESEED_FOUNDATION_UTILITY_STATISTICS_H

// appleseed.foundation headers.
#include "foundation/math/population.h"
#include "foundation/utility/string.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>
#include <ios>
#include <string>
#include <vector>

namespace foundation
{

class Statistics
{
  public:
    explicit Statistics(const std::string& title);

    template <typename T>
    void add(
        const std::string&          name,
        const std::string&          title,
        const T&                    value);

    void add_size(
        const std::string&          name,
        const std::string&          title,
        const uint64                bytes,
        const std::streamsize       precision = 1);

    void add_time(
        const std::string&          name,
        const std::string&          title,
        const double                seconds,
        const std::streamsize       precision = 1);

    template <typename T>
    void add_percent(
        const std::string&          name,
        const std::string&          title,
        const T                     numerator,
        const T                     denominator,
        const std::streamsize       precision = 1);

    template <typename T>
    void add_population(
        const std::string&          name,
        const std::string&          title,
        const Population<T>&        value);

    template <typename T>
    void add_population(
        const std::string&          name,
        const std::string&          title,
        const std::string&          unit,
        const Population<T>&        value);

    std::string to_string(const size_t max_title_length = 16) const;

  private:
    struct Record
    {
        enum Type
        {
            UnsignedInteger,
            UnsignedIntegerPopulation,
            FloatingPoint,
            FloatingPointPopulation,
            String
        };

        const std::string           m_name;
        const std::string           m_title;
        const std::string           m_unit;
        const Type                  m_type;
        const size_t                m_index;

        Record()
          : m_type(Type())
          , m_index(size_t())
        {
        }

        Record(
            const std::string&      name,
            const std::string&      title,
            const Type              type,
            const size_t            index)
          : m_name(name)
          , m_title(title)
          , m_type(type)
          , m_index(index)
        {
        }

        Record(
            const std::string&      name,
            const std::string&      title,
            const std::string&      unit,
            const Type              type,
            const size_t            index)
          : m_name(name)
          , m_title(title)
          , m_unit(unit)
          , m_type(type)
          , m_index(index)
        {
        }
    };

    typedef std::vector<Record> RecordVector;

    const std::string                   m_title;

    RecordVector                        m_records;
    std::vector<size_t>                 m_uint_values;
    std::vector<Population<size_t> >    m_uint_pop_values;
    std::vector<double>                 m_fp_values;
    std::vector<Population<double> >    m_fp_pop_values;
    std::vector<std::string>            m_string_values;
};


//
// Statistics class implementation.
//

template <>
inline void Statistics::add<size_t>(
    const std::string&              name,
    const std::string&              title,
    const size_t&                   value)
{
    const size_t index = m_uint_values.size();
    m_uint_values.push_back(value);
    m_records.push_back(Record(name, title, Record::UnsignedInteger, index));
}

template <>
inline void Statistics::add<double>(
    const std::string&              name,
    const std::string&              title,
    const double&                   value)
{
    const size_t index = m_fp_values.size();
    m_fp_values.push_back(value);
    m_records.push_back(Record(name, title, Record::FloatingPoint, index));
}

template <>
inline void Statistics::add<std::string>(
    const std::string&              name,
    const std::string&              title,
    const std::string&              value)
{
    const size_t index = m_string_values.size();
    m_string_values.push_back(value);
    m_records.push_back(Record(name, title, Record::String, index));
}

inline void Statistics::add_size(
    const std::string&              name,
    const std::string&              title,
    const uint64                    bytes,
    const std::streamsize           precision)
{
    add<std::string>(name, title, pretty_size(bytes, precision));
}

inline void Statistics::add_time(
    const std::string&              name,
    const std::string&              title,
    const double                    seconds,
    const std::streamsize           precision)
{
    add<std::string>(name, title, pretty_time(seconds, precision));
}

template <typename T>
inline void Statistics::add_percent(
    const std::string&              name,
    const std::string&              title,
    const T                         numerator,
    const T                         denominator,
    const std::streamsize           precision)
{
    add<std::string>(name, title, pretty_percent(numerator, denominator, precision));
}

template <typename T>
inline void Statistics::add_population(
    const std::string&              name,
    const std::string&              title,
    const Population<T>&            value)
{
    add_population<T>(name, title, "", value);
}

template <>
inline void Statistics::add_population<size_t>(
    const std::string&              name,
    const std::string&              title,
    const std::string&              unit,
    const Population<size_t>&       value)
{
    const size_t index = m_uint_pop_values.size();
    m_uint_pop_values.push_back(value);
    m_records.push_back(Record(name, title, unit, Record::UnsignedIntegerPopulation, index));
}

template <>
inline void Statistics::add_population<double>(
    const std::string&              name,
    const std::string&              title,
    const std::string&              unit,
    const Population<double>&       value)
{
    const size_t index = m_fp_pop_values.size();
    m_fp_pop_values.push_back(value);
    m_records.push_back(Record(name, title, unit, Record::FloatingPointPopulation, index));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_STATISTICS_H
