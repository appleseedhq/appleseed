
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/core/exceptions/stringexception.h"
#include "foundation/math/population.h"
#include "foundation/utility/string.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>
#include <ios>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace foundation
{

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

    explicit Statistics(const std::string& title);

    ~Statistics();

    template <typename T>
    T* add(
        const std::string&              name,
        const std::string&              title,
        const T&                        value = T());

    template <typename T>
    Population<T>* add_population(
        const std::string&              name,
        const std::string&              title,
        const Population<T>&            value = Population<T>());

    template <typename T>
    Population<T>* add_population(
        const std::string&              name,
        const std::string&              title,
        const std::string&              unit,
        const Population<T>&            value = Population<T>());

    void add_size(
        const std::string&              name,
        const std::string&              title,
        const uint64                    bytes,
        const std::streamsize           precision = 1);

    void add_time(
        const std::string&              name,
        const std::string&              title,
        const double                    seconds,
        const std::streamsize           precision = 1);

    template <typename T>
    void add_percent(
        const std::string&              name,
        const std::string&              title,
        const T                         numerator,
        const T                         denominator,
        const std::streamsize           precision = 1);

    void merge(const Statistics& other);

    std::string to_string(const size_t max_title_length = 16) const;

  private:
    struct RecordBase
    {
        std::string     m_name;
        std::string     m_title;
        std::string     m_unit;

        RecordBase(
            const std::string&          name,
            const std::string&          title,
            const std::string&          unit);

        template <typename T>
        const T* cast(const RecordBase* record);

        virtual std::auto_ptr<RecordBase> clone() const = 0;
        virtual void merge(const RecordBase* other) = 0;
        virtual std::string to_string() const = 0;
    };

    struct UnsignedIntegerRecord
      : public RecordBase
    {
        uint64 m_value;

        UnsignedIntegerRecord(
            const std::string&          name,
            const std::string&          title,
            const std::string&          unit,
            const uint64                value);

        virtual std::auto_ptr<RecordBase> clone() const override;
        virtual void merge(const RecordBase* other) override;
        virtual std::string to_string() const override;
    };

    struct FloatingPointRecord
      : public RecordBase
    {
        double m_value;

        FloatingPointRecord(
            const std::string&          name,
            const std::string&          title,
            const std::string&          unit,
            const double                value);

        virtual std::auto_ptr<RecordBase> clone() const override;
        virtual void merge(const RecordBase* other) override;
        virtual std::string to_string() const override;
    };

    struct StringRecord
      : public RecordBase
    {
        std::string m_value;

        StringRecord(
            const std::string&          name,
            const std::string&          title,
            const std::string&          unit,
            const std::string&          value);

        virtual std::auto_ptr<RecordBase> clone() const override;
        virtual void merge(const RecordBase* other) override;
        virtual std::string to_string() const override;
    };

    struct UnsignedIntegerPopulationRecord
      : public RecordBase
    {
        Population<size_t> m_value;

        UnsignedIntegerPopulationRecord(
            const std::string&          name,
            const std::string&          title,
            const std::string&          unit,
            const Population<size_t>&   value);

        virtual std::auto_ptr<RecordBase> clone() const override;
        virtual void merge(const RecordBase* other) override;
        virtual std::string to_string() const override;
    };

    struct FloatingPointPopulationRecord
      : public RecordBase
    {
        Population<double> m_value;

        FloatingPointPopulationRecord(
            const std::string&          name,
            const std::string&          title,
            const std::string&          unit,
            const Population<double>&   value);

        virtual std::auto_ptr<RecordBase> clone() const override;
        virtual void merge(const RecordBase* other) override;
        virtual std::string to_string() const override;
    };

    typedef std::vector<RecordBase*> RecordVector;
    typedef std::map<std::string, RecordBase*> RecordIndex;

    const std::string                   m_title;

    RecordVector                        m_records;
    RecordIndex                         m_index;

    template <typename T>
    T* cast(const RecordBase* record);

    template <typename T>
    void insert(std::auto_ptr<T> record);
};


//
// Statistics class implementation.
//

template <>
inline uint64* Statistics::add<uint64>(
    const std::string&                  name,
    const std::string&                  title,
    const uint64&                       value)
{
    std::auto_ptr<UnsignedIntegerRecord> record(
        new UnsignedIntegerRecord(name, title, std::string(), value));

    uint64* value_ptr = &record->m_value;

    insert(record);

    return value_ptr;
}

template <>
inline double* Statistics::add<double>(
    const std::string&                  name,
    const std::string&                  title,
    const double&                       value)
{
    std::auto_ptr<FloatingPointRecord> record(
        new FloatingPointRecord(name, title, std::string(), value));

    double* value_ptr = &record->m_value;

    insert(record);

    return value_ptr;
}

template <>
inline std::string* Statistics::add<std::string>(
    const std::string&                  name,
    const std::string&                  title,
    const std::string&                  value)
{
    std::auto_ptr<StringRecord> record(
        new StringRecord(name, title, std::string(), value));

    std::string* value_ptr = &record->m_value;

    insert(record);

    return value_ptr;
}

template <typename T>
Population<T>* Statistics::add_population(
    const std::string&                  name,
    const std::string&                  title,
    const Population<T>&                value)
{
    return add_population<T>(name, title, std::string(), value);
}

template <>
inline Population<size_t>* Statistics::add_population<size_t>(
    const std::string&                  name,
    const std::string&                  title,
    const std::string&                  unit,
    const Population<size_t>&           value)
{
    std::auto_ptr<UnsignedIntegerPopulationRecord> record(
        new UnsignedIntegerPopulationRecord(name, title, unit, value));

    Population<size_t>* value_ptr = &record->m_value;

    insert(record);

    return value_ptr;
}

template <>
inline Population<double>* Statistics::add_population<double>(
    const std::string&                  name,
    const std::string&                  title,
    const std::string&                  unit,
    const Population<double>&           value)
{
    std::auto_ptr<FloatingPointPopulationRecord> record(
        new FloatingPointPopulationRecord(name, title, unit, value));

    Population<double>* value_ptr = &record->m_value;

    insert(record);

    return value_ptr;
}

inline void Statistics::add_size(
    const std::string&                  name,
    const std::string&                  title,
    const uint64                        bytes,
    const std::streamsize               precision)
{
    add<std::string>(name, title, pretty_size(bytes, precision));
}

inline void Statistics::add_time(
    const std::string&                  name,
    const std::string&                  title,
    const double                        seconds,
    const std::streamsize               precision)
{
    add<std::string>(name, title, pretty_time(seconds, precision));
}

template <typename T>
void Statistics::add_percent(
    const std::string&                  name,
    const std::string&                  title,
    const T                             numerator,
    const T                             denominator,
    const std::streamsize               precision)
{
    add<std::string>(name, title, pretty_percent(numerator, denominator, precision));
}

template <typename T>
void Statistics::insert(std::auto_ptr<T> record)
{
    if (m_index.find(record->m_name) != m_index.end())
        throw ExceptionDuplicateName(record->m_name.c_str());

    T* record_ptr = record.release();

    m_records.push_back(record_ptr);
    m_index[record_ptr->m_name] = record_ptr;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_STATISTICS_H
