
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
#include "foundation/core/exceptions/stringexception.h"
#include "foundation/math/population.h"
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <ios>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

namespace foundation
{

//
// A named collection of statistics.
//

class Statistics
{
  public:
    struct ExceptionDuplicateName
      : public StringException
    {
        explicit ExceptionDuplicateName(const char* name);
    };

    struct ExceptionTypeMismatch
      : public StringException
    {
        explicit ExceptionTypeMismatch(const char* name);
    };

    struct Entry
    {
        std::string m_name;
        std::string m_unit;

        explicit Entry(const std::string& name);

        Entry(
            const std::string&          name,
            const std::string&          unit);

        virtual ~Entry() {}

        template <typename T>
        const T* cast(const Entry* entry);

        virtual std::unique_ptr<Entry> clone() const = 0;
        virtual void merge(const Entry* other) = 0;
        virtual std::string to_string() const = 0;
    };

    struct IntegerEntry
      : public Entry
    {
        std::int64_t m_value;

        IntegerEntry(
            const std::string&          name,
            const std::string&          unit,
            const std::int64_t          value);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };

    struct UnsignedIntegerEntry
      : public Entry
    {
        std::uint64_t m_value;

        UnsignedIntegerEntry(
            const std::string&          name,
            const std::string&          unit,
            const std::uint64_t         value);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };

    struct FloatingPointEntry
      : public Entry
    {
        double m_value;

        FloatingPointEntry(
            const std::string&          name,
            const std::string&          unit,
            const double                value);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };

    template <typename T>
    struct PercentEntry
      : public Entry
    {
        T                   m_numerator;
        T                   m_denominator;
        std::streamsize     m_precision;

        PercentEntry(
            const std::string&          name,
            const T                     numerator,
            const T                     denominator,
            const std::streamsize       precision);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };

    struct StringEntry
      : public Entry
    {
        std::string m_value;

        StringEntry(
            const std::string&          name,
            const std::string&          unit,
            const std::string&          value);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };

    template <typename T>
    struct PopulationEntry
      : public Entry
    {
        Population<T>       m_value;
        std::streamsize     m_precision;

        PopulationEntry(
            const std::string&          name,
            const std::string&          unit,
            const Population<T>&        value,
            const std::streamsize       precision);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };

    Statistics();
    Statistics(const Statistics& rhs);

    ~Statistics();

    Statistics& operator=(const Statistics& rhs);

    void clear();

    template <typename T>
    void insert(std::unique_ptr<T> entry);

    template <typename T>
    void insert(
        const std::string&              name,
        const T&                        value);

    template <typename T>
    void insert(
        const std::string&              name,
        const T&                        value,
        const std::string&              unit);

    template <typename T>
    void insert(
        const std::string&              name,
        const Population<T>&            value,
        const std::streamsize           precision = 1);

    template <typename T>
    void insert(
        const std::string&              name,
        const Population<T>&            value,
        const std::string&              unit,
        const std::streamsize           precision = 1);

    void insert_size(
        const std::string&              name,
        const std::uint64_t             bytes,
        const std::streamsize           precision = 1);

    void insert_time(
        const std::string&              name,
        const double                    seconds,
        const std::streamsize           precision = 1);

    template <typename T>
    void insert_percent(
        const std::string&              name,
        const T                         numerator,
        const T                         denominator,
        const std::streamsize           precision = 1);

    void insert(const Statistics& other);

    void merge(const Statistics& other);

    std::string to_string(const size_t max_header_length = 30) const;

  private:
    typedef std::vector<Entry*> EntryVector;
    typedef std::map<std::string, Entry*> EntryIndex;

    EntryVector                         m_entries;
    EntryIndex                          m_index;

    template <typename T>
    T* cast(const Entry* entry);
};


//
// A vector of foundation::Statistics objects.
//

class StatisticsVector
{
  public:
    static StatisticsVector make(
        const std::string&              name,
        const Statistics&               stats);

    void insert(
        const std::string&              name,
        const Statistics&               stats);

    void merge(const StatisticsVector& other);

    std::string to_string(const size_t max_header_length = 30) const;

  private:
    struct NamedStatistics
    {
        std::string     m_name;
        Statistics      m_stats;
    };

    typedef std::vector<NamedStatistics> NamedStatisticsVector;

    NamedStatisticsVector               m_stats;

    void merge(const NamedStatistics& other);
};


//
// Statistics class implementation.
//

template <typename T>
void Statistics::insert(std::unique_ptr<T> entry)
{
    if (m_index.find(entry->m_name) != m_index.end())
        throw ExceptionDuplicateName(entry->m_name.c_str());

    T* entry_ptr = entry.release();

    m_entries.push_back(entry_ptr);
    m_index[entry_ptr->m_name] = entry_ptr;
}

template <typename T>
void Statistics::insert(
    const std::string&                  name,
    const T&                            value)
{
    // Simply defer to the three arguments version.
    insert(name, value, std::string());
}

template <>
inline void Statistics::insert<int>(
    const std::string&                  name,
    const int&                          value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<IntegerEntry>(
            new IntegerEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<unsigned int>(
    const std::string&                  name,
    const unsigned int&                 value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<UnsignedIntegerEntry>(
            new UnsignedIntegerEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<long>(
    const std::string&                  name,
    const long&                         value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<IntegerEntry>(
            new IntegerEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<unsigned long>(
    const std::string&                  name,
    const unsigned long&                value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<UnsignedIntegerEntry>(
            new UnsignedIntegerEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<long long>(
    const std::string&                  name,
    const long long&                    value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<IntegerEntry>(
            new IntegerEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<unsigned long long>(
    const std::string&                  name,
    const unsigned long long&           value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<UnsignedIntegerEntry>(
            new UnsignedIntegerEntry(name, unit, value)));
}
template <>
inline void Statistics::insert<float>(
    const std::string&                  name,
    const float&                        value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<FloatingPointEntry>(
            new FloatingPointEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<double>(
    const std::string&                  name,
    const double&                       value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<FloatingPointEntry>(
            new FloatingPointEntry(name, unit, value)));
}

template <>
inline void Statistics::insert<std::string>(
    const std::string&                  name,
    const std::string&                  value,
    const std::string&                  unit)
{
    insert(
        std::unique_ptr<StringEntry>(
            new StringEntry(name, unit, value)));
}

template <typename T>
void Statistics::insert(
    const std::string&                  name,
    const Population<T>&                value,
    const std::streamsize               precision)
{
    insert(name, value, std::string(), precision);
}

template <typename T>
inline void Statistics::insert(
    const std::string&                  name,
    const Population<T>&                value,
    const std::string&                  unit,
    const std::streamsize               precision)
{
    insert(
        std::unique_ptr<PopulationEntry<T>>(
            new PopulationEntry<T>(name, unit, value, precision)));
}

inline void Statistics::insert_size(
    const std::string&                  name,
    const std::uint64_t                 bytes,
    const std::streamsize               precision)
{
    insert(name, pretty_size(bytes, precision));
}

inline void Statistics::insert_time(
    const std::string&                  name,
    const double                        seconds,
    const std::streamsize               precision)
{
    insert(name, pretty_time(seconds, precision));
}

template <typename T>
void Statistics::insert_percent(
    const std::string&                  name,
    const T                             numerator,
    const T                             denominator,
    const std::streamsize               precision)
{
    insert(
        std::unique_ptr<PercentEntry<T>>(
            new PercentEntry<T>(name, numerator, denominator, precision)));
}


//
// Statistics::Entry class implementation.
//

template <typename T>
const T* Statistics::Entry::cast(const Entry* entry)
{
    assert(entry);

    const T* typed_entry = dynamic_cast<const T*>(entry);

    if (typed_entry == nullptr)
        throw ExceptionTypeMismatch(entry->m_name.c_str());

    return typed_entry;
}


//
// Statistics::PercentEntry class implementation.
//

template <typename T>
Statistics::PercentEntry<T>::PercentEntry(
    const std::string&                  name,
    const T                             numerator,
    const T                             denominator,
    const std::streamsize               precision)
  : Entry(name)
  , m_numerator(numerator)
  , m_denominator(denominator)
  , m_precision(precision)
{
}

template <typename T>
std::unique_ptr<Statistics::Entry> Statistics::PercentEntry<T>::clone() const
{
    return std::unique_ptr<Statistics::Entry>(new PercentEntry(*this));
}

template <typename T>
void Statistics::PercentEntry<T>::merge(const Entry* other)
{
    const PercentEntry* pother = cast<PercentEntry>(other);

    m_numerator += pother->m_numerator;
    m_denominator += pother->m_denominator;
    m_precision = std::max(m_precision, pother->m_precision);
}

template <typename T>
std::string Statistics::PercentEntry<T>::to_string() const
{
    return pretty_percent(m_numerator, m_denominator, m_precision);
}


//
// Statistics::PopulationEntry class implementation.
//

template <typename T>
Statistics::PopulationEntry<T>::PopulationEntry(
    const std::string&                  name,
    const std::string&                  unit,
    const Population<T>&                value,
    const std::streamsize               precision)
  : Entry(name, unit)
  , m_value(value)
  , m_precision(precision)
{
}

template <typename T>
std::unique_ptr<Statistics::Entry> Statistics::PopulationEntry<T>::clone() const
{
    return std::unique_ptr<Statistics::Entry>(new PopulationEntry(*this));
}

template <typename T>
void Statistics::PopulationEntry<T>::merge(const Entry* other)
{
    m_value.merge(cast<PopulationEntry>(other)->m_value);
    m_precision = std::max(m_precision, cast<PopulationEntry>(other)->m_precision);
}

template <typename T>
std::string Statistics::PopulationEntry<T>::to_string() const
{
    std::stringstream sstr;
    sstr << std::fixed << std::setprecision(m_precision);

    sstr <<   "avg " << m_value.get_mean() << m_unit;
    sstr << "  min " << m_value.get_min() << m_unit;
    sstr << "  max " << m_value.get_max() << m_unit;
    sstr << "  dev " << m_value.get_dev() << m_unit;

    return sstr.str();
}

}   // namespace foundation
