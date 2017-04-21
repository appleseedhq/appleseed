
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

// Interface header.
#include "statistics.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

using namespace std;

namespace foundation
{

//
// Statistics class implementation.
//

Statistics::Statistics()
{
}

Statistics::Statistics(const Statistics& rhs)
{
    insert(rhs);
}

Statistics::~Statistics()
{
    clear();
}

Statistics& Statistics::operator=(const Statistics& rhs)
{
    clear();
    insert(rhs);

    return *this;
}

void Statistics::clear()
{
    for (const_each<EntryVector> i = m_entries; i; ++i)
        delete *i;

    m_entries.clear();
    m_index.clear();
}

void Statistics::insert(const Statistics& other)
{
    for (const_each<EntryVector> i = other.m_entries; i; ++i)
    {
        const Entry* other_entry = *i;
        insert(other_entry->clone());
    }
}

void Statistics::merge(const Statistics& other)
{
    for (const_each<EntryVector> i = other.m_entries; i; ++i)
    {
        const Entry* other_entry = *i;
        const EntryIndex::iterator it = m_index.find(other_entry->m_name);

        if (it == m_index.end())
            insert(other_entry->clone());
        else it->second->merge(other_entry);
    }
}

string Statistics::to_string(const size_t max_header_length) const
{
    if (m_entries.empty())
        return "  no statistics";

    stringstream sstr;

    for (const_each<EntryVector> i = m_entries; i; ++i)
    {
        const Entry* entry = *i;

        if (i.it() > m_entries.begin())
            sstr << endl;

        sstr << "  ";

        if (entry->m_name.size() > max_header_length)
            sstr << entry->m_name.substr(0, max_header_length);
        else sstr << entry->m_name << string(max_header_length - entry->m_name.size(), ' ');

        sstr << entry->to_string();
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
// Statistics::Entry class implementation.
//

Statistics::Entry::Entry(const string& name)
  : m_name(name)
{
}

Statistics::Entry::Entry(
    const string&               name,
    const string&               unit)
  : m_name(name)
  , m_unit(unit)
{
}


//
// Statistics::IntegerEntry class implementation.
//

Statistics::IntegerEntry::IntegerEntry(
    const string&               name,
    const string&               unit,
    const int64                 value)
  : Entry(name, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::Entry> Statistics::IntegerEntry::clone() const
{
    return auto_ptr<Entry>(new IntegerEntry(*this));
}

void Statistics::IntegerEntry::merge(const Entry* other)
{
    m_value += cast<IntegerEntry>(other)->m_value;
}

string Statistics::IntegerEntry::to_string() const
{
    return pretty_int(m_value);
}


//
// Statistics::UnsignedIntegerEntry class implementation.
//

Statistics::UnsignedIntegerEntry::UnsignedIntegerEntry(
    const string&               name,
    const string&               unit,
    const uint64                value)
  : Entry(name, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::Entry> Statistics::UnsignedIntegerEntry::clone() const
{
    return auto_ptr<Entry>(new UnsignedIntegerEntry(*this));
}

void Statistics::UnsignedIntegerEntry::merge(const Entry* other)
{
    m_value += cast<UnsignedIntegerEntry>(other)->m_value;
}

string Statistics::UnsignedIntegerEntry::to_string() const
{
    return pretty_uint(m_value);
}


//
// Statistics::FloatingPointEntry class implementation.
//

Statistics::FloatingPointEntry::FloatingPointEntry(
    const string&               name,
    const string&               unit,
    const double                value)
  : Entry(name, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::Entry> Statistics::FloatingPointEntry::clone() const
{
    return auto_ptr<Entry>(new FloatingPointEntry(*this));
}

void Statistics::FloatingPointEntry::merge(const Entry* other)
{
    m_value += cast<FloatingPointEntry>(other)->m_value;
}

string Statistics::FloatingPointEntry::to_string() const
{
    return pretty_scalar(m_value);
}


//
// Statistics::StringEntry class implementation.
//

Statistics::StringEntry::StringEntry(
    const string&               name,
    const string&               unit,
    const string&               value)
  : Entry(name, unit)
  , m_value(value)
{
}

auto_ptr<Statistics::Entry> Statistics::StringEntry::clone() const
{
    return auto_ptr<Entry>(new StringEntry(*this));
}

void Statistics::StringEntry::merge(const Entry* other)
{
    // String statistics are not merged.
}

string Statistics::StringEntry::to_string() const
{
    return m_value;
}


//
// StatisticsVector class implementation.
//

StatisticsVector StatisticsVector::make(
    const string&               name,
    const Statistics&           stats)
{
    StatisticsVector vec;
    vec.insert(name, stats);
    return vec;
}

void StatisticsVector::insert(
    const string&               name,
    const Statistics&           stats)
{
    NamedStatistics named_stats;
    named_stats.m_name = name;
    named_stats.m_stats = stats;
    m_stats.push_back(named_stats);
}

void StatisticsVector::merge(const StatisticsVector& other)
{
    for (const_each<NamedStatisticsVector> i = other.m_stats; i; ++i)
        merge(*i);
}

void StatisticsVector::merge(const NamedStatistics& other)
{
    for (each<NamedStatisticsVector> i = m_stats; i; ++i)
    {
        if (i->m_name == other.m_name)
        {
            i->m_stats.merge(other.m_stats);
            return;
        }
    }

    m_stats.push_back(other);
}

string StatisticsVector::to_string(const size_t max_header_length) const
{
    stringstream sstr;

    for (const_each<NamedStatisticsVector> i = m_stats; i; ++i)
    {
        if (i.it() > m_stats.begin())
            sstr << endl;

        sstr << i->m_name << ":" << endl;
        sstr << i->m_stats.to_string(max_header_length);
    }

    return sstr.str();
}

}   // namespace foundation
