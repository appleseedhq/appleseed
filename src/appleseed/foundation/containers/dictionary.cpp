
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

// Interface header.
#include "dictionary.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <map>
#include <string>

namespace foundation
{

typedef std::map<InternedString, std::string> StringMap;
typedef std::map<InternedString, Dictionary> DictionaryMap;


//
// StringDictionary::const_iterator class implementation.
//

struct StringDictionary::const_iterator::Impl
{
    StringMap::const_iterator m_it;
};

StringDictionary::const_iterator::const_iterator()
  : impl(new Impl())
{
}

StringDictionary::const_iterator::const_iterator(const const_iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

StringDictionary::const_iterator::~const_iterator()
{
    delete impl;
}

StringDictionary::const_iterator& StringDictionary::const_iterator::operator=(const const_iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool StringDictionary::const_iterator::operator==(const const_iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool StringDictionary::const_iterator::operator!=(const const_iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

StringDictionary::const_iterator& StringDictionary::const_iterator::operator++()
{
    ++impl->m_it;
    return *this;
}

StringDictionary::const_iterator& StringDictionary::const_iterator::operator--()
{
    --impl->m_it;
    return *this;
}

const StringDictionary::const_iterator::value_type& StringDictionary::const_iterator::operator*() const
{
    return *this;
}

const char* StringDictionary::const_iterator::key() const
{
    return impl->m_it->first.c_str();
}

const char* StringDictionary::const_iterator::value() const
{
    return impl->m_it->second.c_str();
}


//
// StringDictionary class implementation.
//

struct StringDictionary::Impl
{
    StringMap m_strings;
};

StringDictionary::StringDictionary()
  : impl(new Impl())
{
}

StringDictionary::StringDictionary(const StringDictionary& rhs)
  : impl(new Impl(*rhs.impl))
{
}

StringDictionary::~StringDictionary()
{
    delete impl;
}

StringDictionary& StringDictionary::operator=(const StringDictionary& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool StringDictionary::operator==(const StringDictionary& rhs) const
{
    if (size() != rhs.size())
        return false;

    for (
        StringMap::const_iterator it = impl->m_strings.begin(), rhs_it = rhs.impl->m_strings.begin();
        it != impl->m_strings.end();
        ++it, ++rhs_it)
    {
        if (it->first != rhs_it->first || it->second != rhs_it->second)
            return false;
    }

    return true;
}

bool StringDictionary::operator!=(const StringDictionary& rhs) const
{
    return !(*this == rhs);
}

size_t StringDictionary::size() const
{
    return impl->m_strings.size();
}

bool StringDictionary::empty() const
{
    return impl->m_strings.empty();
}

void StringDictionary::clear()
{
    impl->m_strings.clear();
}

StringDictionary& StringDictionary::insert(const char* key, const char* value)
{
    assert(key);
    assert(value);

    impl->m_strings[InternedString(key)] = value;

    return *this;
}

StringDictionary& StringDictionary::set(const char* key, const char* value)
{
    assert(key);
    assert(value);

    const StringMap::iterator i = impl->m_strings.find(InternedString(key));

    if (i == impl->m_strings.end())
        throw ExceptionDictionaryKeyNotFound(key);

    i->second = value;

    return *this;
}

const char* StringDictionary::get(const char* key) const
{
    assert(key);

    const StringMap::const_iterator i = impl->m_strings.find(InternedString(key));

    if (i == impl->m_strings.end())
        throw ExceptionDictionaryKeyNotFound(key);

    return i->second.c_str();
}

bool StringDictionary::exist(const char* key) const
{
    assert(key);

    return impl->m_strings.find(InternedString(key)) != impl->m_strings.end();
}

StringDictionary& StringDictionary::remove(const char* key)
{
    assert(key);

    const StringMap::iterator i = impl->m_strings.find(InternedString(key));

    if (i != impl->m_strings.end())
        impl->m_strings.erase(i);

    return *this;
}

StringDictionary::const_iterator StringDictionary::begin() const
{
    const_iterator it;
    it.impl->m_it = impl->m_strings.begin();
    return it;
}

StringDictionary::const_iterator StringDictionary::end() const
{
    const_iterator it;
    it.impl->m_it = impl->m_strings.end();
    return it;
}


//
// DictionaryDictionary::iterator class implementation.
//

struct DictionaryDictionary::iterator::Impl
{
    DictionaryMap::iterator m_it;
};

DictionaryDictionary::iterator::iterator()
  : impl(new Impl())
{
}

DictionaryDictionary::iterator::iterator(const iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

DictionaryDictionary::iterator::~iterator()
{
    delete impl;
}

DictionaryDictionary::iterator& DictionaryDictionary::iterator::operator=(const iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool DictionaryDictionary::iterator::operator==(const iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool DictionaryDictionary::iterator::operator!=(const iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

DictionaryDictionary::iterator& DictionaryDictionary::iterator::operator++()
{
    ++impl->m_it;
    return *this;
}

DictionaryDictionary::iterator& DictionaryDictionary::iterator::operator--()
{
    --impl->m_it;
    return *this;
}

DictionaryDictionary::iterator::value_type& DictionaryDictionary::iterator::operator*()
{
    return *this;
}

const char* DictionaryDictionary::iterator::key() const
{
    return impl->m_it->first.c_str();
}

Dictionary& DictionaryDictionary::iterator::value()
{
    return impl->m_it->second;
}


//
// DictionaryDictionary::const_iterator class implementation.
//

struct DictionaryDictionary::const_iterator::Impl
{
    DictionaryMap::const_iterator m_it;
};

DictionaryDictionary::const_iterator::const_iterator()
  : impl(new Impl())
{
}

DictionaryDictionary::const_iterator::const_iterator(const const_iterator& rhs)
  : impl(new Impl())
{
    impl->m_it = rhs.impl->m_it;
}

DictionaryDictionary::const_iterator::const_iterator(const iterator& rhs)
  : impl(new Impl())
{
    impl->m_it = rhs.impl->m_it;
}

DictionaryDictionary::const_iterator::~const_iterator()
{
    delete impl;
}

DictionaryDictionary::const_iterator& DictionaryDictionary::const_iterator::operator=(const const_iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool DictionaryDictionary::const_iterator::operator==(const const_iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool DictionaryDictionary::const_iterator::operator!=(const const_iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

DictionaryDictionary::const_iterator& DictionaryDictionary::const_iterator::operator++()
{
    ++impl->m_it;
    return *this;
}

DictionaryDictionary::const_iterator& DictionaryDictionary::const_iterator::operator--()
{
    --impl->m_it;
    return *this;
}

const DictionaryDictionary::const_iterator::value_type& DictionaryDictionary::const_iterator::operator*() const
{
    return *this;
}

const char* DictionaryDictionary::const_iterator::key() const
{
    return impl->m_it->first.c_str();
}

const Dictionary& DictionaryDictionary::const_iterator::value() const
{
    return impl->m_it->second;
}


//
// DictionaryDictionary class implementation.
//

struct DictionaryDictionary::Impl
{
    DictionaryMap m_dictionaries;
};

DictionaryDictionary::DictionaryDictionary()
  : impl(new Impl())
{
}

DictionaryDictionary::DictionaryDictionary(const DictionaryDictionary& rhs)
  : impl(new Impl(*rhs.impl))
{
}

DictionaryDictionary::~DictionaryDictionary()
{
    delete impl;
}

DictionaryDictionary& DictionaryDictionary::operator=(const DictionaryDictionary& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool DictionaryDictionary::operator==(const DictionaryDictionary& rhs) const
{
    if (size() != rhs.size())
        return false;

    for (
        DictionaryMap::const_iterator it = impl->m_dictionaries.begin(), rhs_it = rhs.impl->m_dictionaries.begin();
        it != impl->m_dictionaries.end();
        ++it, ++rhs_it)
    {
        if (it->first != rhs_it->first || it->second != rhs_it->second)
            return false;
    }

    return true;
}

bool DictionaryDictionary::operator!=(const DictionaryDictionary& rhs) const
{
    return !(*this == rhs);
}

size_t DictionaryDictionary::size() const
{
    return impl->m_dictionaries.size();
}

bool DictionaryDictionary::empty() const
{
    return impl->m_dictionaries.empty();
}

void DictionaryDictionary::clear()
{
    impl->m_dictionaries.clear();
}

DictionaryDictionary& DictionaryDictionary::insert(const char* key, const Dictionary& value)
{
    assert(key);

    impl->m_dictionaries[InternedString(key)] = value;

    return *this;
}

DictionaryDictionary& DictionaryDictionary::set(const char* key, const Dictionary& value)
{
    assert(key);

    const DictionaryMap::iterator i = impl->m_dictionaries.find(InternedString(key));

    if (i == impl->m_dictionaries.end())
        throw ExceptionDictionaryKeyNotFound(key);

    i->second = value;

    return *this;
}

Dictionary& DictionaryDictionary::get(const char* key)
{
    assert(key);

    const DictionaryMap::iterator i = impl->m_dictionaries.find(InternedString(key));

    if (i == impl->m_dictionaries.end())
        throw ExceptionDictionaryKeyNotFound(key);

    return i->second;
}

const Dictionary& DictionaryDictionary::get(const char* key) const
{
    assert(key);

    const DictionaryMap::const_iterator i = impl->m_dictionaries.find(InternedString(key));

    if (i == impl->m_dictionaries.end())
        throw ExceptionDictionaryKeyNotFound(key);

    return i->second;
}

bool DictionaryDictionary::exist(const char* key) const
{
    assert(key);

    return impl->m_dictionaries.find(InternedString(key)) != impl->m_dictionaries.end();
}

DictionaryDictionary& DictionaryDictionary::remove(const char* key)
{
    assert(key);

    const DictionaryMap::iterator i = impl->m_dictionaries.find(InternedString(key));

    if (i != impl->m_dictionaries.end())
        impl->m_dictionaries.erase(i);

    return *this;
}

DictionaryDictionary::iterator DictionaryDictionary::begin()
{
    iterator it;
    it.impl->m_it = impl->m_dictionaries.begin();
    return it;
}

DictionaryDictionary::iterator DictionaryDictionary::end()
{
    iterator it;
    it.impl->m_it = impl->m_dictionaries.end();
    return it;
}

DictionaryDictionary::const_iterator DictionaryDictionary::begin() const
{
    const_iterator it;
    it.impl->m_it = impl->m_dictionaries.begin();
    return it;
}

DictionaryDictionary::const_iterator DictionaryDictionary::end() const
{
    const_iterator it;
    it.impl->m_it = impl->m_dictionaries.end();
    return it;
}


//
// Dictionary class implementation.
//

Dictionary& Dictionary::merge(const Dictionary& rhs)
{
    // Merge strings.
    for (const_each<StringDictionary> i = rhs.strings(); i; ++i)
        insert(i->key(), i->value());

    // Recursively merge dictionaries.
    for (const_each<DictionaryDictionary> i = rhs.dictionaries(); i; ++i)
    {
        if (dictionaries().exist(i->key()))
            dictionary(i->key()).merge(i->value());
        else insert(i->key(), i->value());
    }

    return *this;
}

}   // namespace foundation
