
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

// Interface header.
#include "statistics.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <iomanip>
#include <sstream>

using namespace std;

namespace foundation
{

//
// Statistics class implementation.
//

Statistics::Statistics(const string& title)
  : m_title(title)
{
}

Statistics::~Statistics()
{
    for (const_each<RecordVector> i = m_records; i; ++i)
        delete *i;
}

void Statistics::merge(const Statistics& other)
{
    for (const_each<RecordVector> i = other.m_records; i; ++i)
    {
        const RecordBase* other_record = *i;
        const RecordIndex::iterator it = m_index.find(other_record->m_name);

        if (it == m_index.end())
            insert(other_record->clone());
        else it->second->merge(other_record);
    }
}

namespace
{
    template <typename T>
    string pop_to_string(const Population<T>& pop, const string& unit)
    {
        stringstream sstr;
        sstr << fixed << setprecision(1);

        sstr <<   "avg " << pop.get_mean() << unit;
        sstr << "  min " << pop.get_min() << unit;
        sstr << "  max " << pop.get_max() << unit;
        sstr << "  dev " << pop.get_dev() << unit;

        return sstr.str();
    }
}

string Statistics::to_string(const size_t max_title_length) const
{
    stringstream sstr;

    sstr << m_title << ":" << endl;

    if (m_records.empty())
    {
        sstr << "  no statistics" << endl;
    }
    else
    {
        for (const_each<RecordVector> i = m_records; i; ++i)
        {
            const RecordBase* record = *i;

            sstr << "  ";

            if (record->m_title.size() > max_title_length)
                sstr << record->m_title.substr(0, max_title_length);
            else sstr << record->m_title << string(max_title_length - record->m_title.size(), ' ');

            sstr << ' ' << record->to_string() << endl;
        }
    }

    return sstr.str();
}


//
// Statistics::ExceptionDuplicateName class implementation.
//

Statistics::ExceptionDuplicateName::ExceptionDuplicateName(const char* name)
  : StringException("a statistic with this name already exists", name)
{
}


//
// Statistics::ExceptionTypeMismatch class implementation.
//

Statistics::ExceptionTypeMismatch::ExceptionTypeMismatch(const char* name)
  : StringException("type mismatch while merging this statistic", name)
{
}


//
// Statistics::RecordBase class implementation.
//

Statistics::RecordBase::RecordBase(
    const string&               name,
    const string&               title,
    const string&               unit)
  : m_name(name)
  , m_title(title)
  , m_unit(unit)
{
}

template <typename T>
const T* Statistics::RecordBase::cast(const RecordBase* record)
{
    assert(record);

    const T* typed_record = dynamic_cast<const T*>(record);

    if (typed_record == 0)
        throw ExceptionTypeMismatch(record->m_name.c_str());

    return typed_record;
}


//
// Statistics::UnsignedIntegerRecord class implementation.
//

Statistics::UnsignedIntegerRecord::UnsignedIntegerRecord(
    const string&               name,
    const string&               title,
    const string&               unit,
    const uint64                value)
  : RecordBase(name, title, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::RecordBase> Statistics::UnsignedIntegerRecord::clone() const
{
    return auto_ptr<RecordBase>(new UnsignedIntegerRecord(*this));
}

void Statistics::UnsignedIntegerRecord::merge(const RecordBase* other)
{
    m_value += cast<UnsignedIntegerRecord>(other)->m_value;
}

string Statistics::UnsignedIntegerRecord::to_string() const
{
    return pretty_uint(m_value);
}


//
// Statistics::FloatingPointRecord class implementation.
//

Statistics::FloatingPointRecord::FloatingPointRecord(
    const string&               name,
    const string&               title,
    const string&               unit,
    const double                value)
  : RecordBase(name, title, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::RecordBase> Statistics::FloatingPointRecord::clone() const
{
    return auto_ptr<RecordBase>(new FloatingPointRecord(*this));
}

void Statistics::FloatingPointRecord::merge(const RecordBase* other)
{
    m_value += cast<FloatingPointRecord>(other)->m_value;
}

string Statistics::FloatingPointRecord::to_string() const
{
    return pretty_scalar(m_value);
}


//
// Statistics::StringRecord class implementation.
//

Statistics::StringRecord::StringRecord(
    const string&               name,
    const string&               title,
    const string&               unit,
    const string&               value)
  : RecordBase(name, title, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::RecordBase> Statistics::StringRecord::clone() const
{
    return auto_ptr<RecordBase>(new StringRecord(*this));
}

void Statistics::StringRecord::merge(const RecordBase* other)
{
    // String statistics are not merged.
}

string Statistics::StringRecord::to_string() const
{
    return m_value;
}


//
// Statistics::UnsignedIntegerPopulationRecord class implementation.
//

Statistics::UnsignedIntegerPopulationRecord::UnsignedIntegerPopulationRecord(
    const string&               name,
    const string&               title,
    const string&               unit,
    const Population<size_t>&   value)
  : RecordBase(name, title, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::RecordBase> Statistics::UnsignedIntegerPopulationRecord::clone() const
{
    return auto_ptr<RecordBase>(new UnsignedIntegerPopulationRecord(*this));
}

void Statistics::UnsignedIntegerPopulationRecord::merge(const RecordBase* other)
{
    m_value.merge(cast<UnsignedIntegerPopulationRecord>(other)->m_value);
}

string Statistics::UnsignedIntegerPopulationRecord::to_string() const
{
    return pop_to_string(m_value, m_unit);
}


//
// Statistics::FloatingPointPopulationRecord class implementation.
//

Statistics::FloatingPointPopulationRecord::FloatingPointPopulationRecord(
    const string&               name,
    const string&               title,
    const string&               unit,
    const Population<double>&   value)
  : RecordBase(name, title, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::RecordBase> Statistics::FloatingPointPopulationRecord::clone() const
{
    return auto_ptr<RecordBase>(new FloatingPointPopulationRecord(*this));
}

void Statistics::FloatingPointPopulationRecord::merge(const RecordBase* other)
{
    m_value.merge(cast<FloatingPointPopulationRecord>(other)->m_value);
}

string Statistics::FloatingPointPopulationRecord::to_string() const
{
    return pop_to_string(m_value, m_unit);
}

}   // namespace foundation
