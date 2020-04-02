
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
#include "foundation/string/internedstring.h"
#include "foundation/string/string.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Dictionary; }

namespace foundation
{

//
// The exception thrown when accessing a non-existing dictionary key.
//

struct ExceptionDictionaryKeyNotFound
  : public StringException
{
    explicit ExceptionDictionaryKeyNotFound(const char* key)
      : StringException("dictionary key not found", key)
    {
    }
};


//
// A string-to-string dictionary that can cross DLL boundaries.
//

class APPLESEED_DLLSYMBOL StringDictionary
{
  public:
    class APPLESEED_DLLSYMBOL const_iterator
    {
      public:
        // Value type.
        typedef const_iterator value_type;

        // Constructors.
        const_iterator();
        const_iterator(const const_iterator& rhs);

        // Destructor.
        ~const_iterator();

        // Assignment operator.
        const_iterator& operator=(const const_iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const const_iterator& rhs) const;
        bool operator!=(const const_iterator& rhs) const;

        // Preincrement and predecrement operators.
        const_iterator& operator++();
        const_iterator& operator--();

        // Dereference operator.
        const value_type& operator*() const;

        // Get the key of this item.
        const char* key() const;

        // Get the value of this item.
        const char* value() const;
        template <typename T> T value() const;

      private:
        friend class StringDictionary;

        struct Impl;
        Impl* impl;
    };

    // Constructors.
    StringDictionary();
    StringDictionary(const StringDictionary& rhs);

    // Destructor.
    ~StringDictionary();

    // Assignment operator.
    StringDictionary& operator=(const StringDictionary& rhs);

    // Comparison operators.
    bool operator==(const StringDictionary& rhs) const;
    bool operator!=(const StringDictionary& rhs) const;

    // Return the number of items in the dictionary.
    size_t size() const;

    // Return true if the dictionary is empty.
    bool empty() const;

    // Remove all items from the dictionary.
    void clear();

    // Insert an item into the dictionary, replacing the previous item if one exists for that key.
    // Returns the dictionary itself to allow chaining of operations.
    StringDictionary& insert(const char* key, const char* value);
    template <typename T> StringDictionary& insert(const char* key, const T& value);

    // Set the value of an existing item.
    // Returns the dictionary itself to allow chaining of operations.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    StringDictionary& set(const char* key, const char* value);
    template <typename T> StringDictionary& set(const char* key, const T& value);

    // Retrieve an item from the dictionary.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    const char* get(const char* key) const;
    template <typename T> T get(const char* key) const;

    // Return true if an item with a given key exists in the dictionary.
    bool exist(const char* key) const;

    // Remove an item from the dictionary, if it exists.
    // Returns the dictionary itself to allow chaining of operations.
    StringDictionary& remove(const char* key);

    // Return constant begin and end input iterators.
    const_iterator begin() const;
    const_iterator end() const;

  private:
    struct Impl;
    Impl* impl;
};


//
// A string-to-dictionary dictionary that can cross DLL boundaries.
//

class APPLESEED_DLLSYMBOL DictionaryDictionary
{
  public:
    class iterator;

    // Constant iterator.
    class APPLESEED_DLLSYMBOL const_iterator
    {
      public:
        // Value type.
        typedef const_iterator value_type;

        // Constructors.
        const_iterator();
        const_iterator(const const_iterator& rhs);
        const_iterator(const iterator& rhs);

        // Destructor.
        ~const_iterator();

        // Assignment operator.
        const_iterator& operator=(const const_iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const const_iterator& rhs) const;
        bool operator!=(const const_iterator& rhs) const;

        // Preincrement and predecrement operators.
        const_iterator& operator++();
        const_iterator& operator--();

        // Dereference operator.
        const value_type& operator*() const;

        // Get the key of this item.
        const char* key() const;

        // Get the value of this item.
        const Dictionary& value() const;

      private:
        friend class DictionaryDictionary;

        struct Impl;
        Impl* impl;
    };

    // Mutable iterator.
    class APPLESEED_DLLSYMBOL iterator
    {
      public:
        // Value type.
        typedef iterator value_type;

        // Constructors.
        iterator();
        iterator(const iterator& rhs);

        // Destructor.
        ~iterator();

        // Assignment operator.
        iterator& operator=(const iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const iterator& rhs) const;
        bool operator!=(const iterator& rhs) const;

        // Preincrement and predecrement operators.
        iterator& operator++();
        iterator& operator--();

        // Dereference operator.
        value_type& operator*();

        // Get the key of this item.
        const char* key() const;

        // Get the value of this item.
        Dictionary& value();

      private:
        friend class DictionaryDictionary;

        struct Impl;
        Impl* impl;
    };

    // Constructors.
    DictionaryDictionary();
    DictionaryDictionary(const DictionaryDictionary& rhs);

    // Destructor.
    ~DictionaryDictionary();

    // Assignment operator.
    DictionaryDictionary& operator=(const DictionaryDictionary& rhs);

    // Comparison operators.
    bool operator==(const DictionaryDictionary& rhs) const;
    bool operator!=(const DictionaryDictionary& rhs) const;

    // Return the number of items in the dictionary.
    size_t size() const;

    // Return true if the dictionary is empty.
    bool empty() const;

    // Remove all items from the dictionary.
    void clear();

    // Insert an item into the dictionary, replacing the previous item if one exists for that key.
    // Returns the dictionary itself to allow chaining of operations.
    DictionaryDictionary& insert(const char* key, const Dictionary& value);

    // Set the value of an existing item.
    // Returns the dictionary itself to allow chaining of operations.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    DictionaryDictionary& set(const char* key, const Dictionary& value);

    // Retrieve an item from the dictionary.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    Dictionary& get(const char* key);
    const Dictionary& get(const char* key) const;

    // Return true if an item with a given key exists in the dictionary.
    bool exist(const char* key) const;

    // Remove an item from the dictionary, if it exists.
    // Returns the dictionary itself to allow chaining of operations.
    DictionaryDictionary& remove(const char* key);

    // Return mutable begin and end input iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end input iterators.
    const_iterator begin() const;
    const_iterator end() const;

  private:
    struct Impl;
    Impl* impl;
};


//
// A dictionary that supports nesting and that can cross DLL boundaries.
//

class APPLESEED_DLLSYMBOL Dictionary
{
  public:
    // Comparison operators.
    bool operator==(const Dictionary& rhs) const;
    bool operator!=(const Dictionary& rhs) const;

    // Return the number of items in the dictionary.
    size_t size() const;

    // Return true if the dictionary is empty.
    bool empty() const;

    // Remove all items from the dictionary.
    void clear();

    // Insert an item into the dictionary, replacing the previous item if one exists for that key.
    // Returns the dictionary itself to allow chaining of operations.
    Dictionary& insert(const char* key, const char* value);
    template <typename T> Dictionary& insert(const char* key, const T& value);

    // Set the value of an existing item.
    // Returns the dictionary itself to allow chaining of operations.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    Dictionary& set(const char* key, const char* value);
    template <typename T> Dictionary& set(const char* key, const T& value);

    // Retrieve a string item from the dictionary.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    const char* get(const char* key) const;
    template <typename T> T get(const char* key) const;

    // Access a child dictionary.
    // Throws a ExceptionDictionaryKeyNotFound exception if the item could not be found.
    Dictionary& dictionary(const char* key);
    const Dictionary& dictionary(const char* key) const;

    // Access the string dictionary.
    StringDictionary& strings();
    const StringDictionary& strings() const;

    // Access the dictionary dictionary.
    DictionaryDictionary& dictionaries();
    const DictionaryDictionary& dictionaries() const;

    // Merge another dictionary into this one.
    // Returns the dictionary itself to allow chaining of operations.
    Dictionary& merge(const Dictionary& rhs);

  private:
    StringDictionary        m_strings;
    DictionaryDictionary    m_dictionaries;
};


//
// StringDictionary::const_iterator class implementation.
//

template <typename T>
inline T StringDictionary::const_iterator::value() const
{
    return from_string<T>(value());
}


//
// StringDictionary class implementation.
//

template <typename T>
inline StringDictionary& StringDictionary::insert(const char* key, const T& value)
{
    return insert(key, to_string(value).c_str());
}

template <typename T>
inline StringDictionary& StringDictionary::set(const char* key, const T& value)
{
    return set(key, to_string(value).c_str());
}

template <typename T>
inline T StringDictionary::get(const char* key) const
{
    return from_string<T>(get(key));
}


//
// Dictionary class implementation.
//

inline bool Dictionary::operator==(const Dictionary& rhs) const
{
    return m_strings == rhs.m_strings && m_dictionaries == rhs.m_dictionaries;
}

inline bool Dictionary::operator!=(const Dictionary& rhs) const
{
    return !(*this == rhs);
}

inline size_t Dictionary::size() const
{
    return m_strings.size() + m_dictionaries.size();
}

inline bool Dictionary::empty() const
{
    return m_strings.empty() && m_dictionaries.empty();
}

inline void Dictionary::clear()
{
    m_strings.clear();
    m_dictionaries.clear();
}

inline Dictionary& Dictionary::insert(const char* key, const char* value)
{
    m_strings.insert(key, value);
    return *this;
}

template <typename T>
inline Dictionary& Dictionary::insert(const char* key, const T& value)
{
    return insert(key, to_string(value).c_str());
}

template <>
inline Dictionary& Dictionary::insert(const char* key, const Dictionary& value)
{
    m_dictionaries.insert(key, value);
    return *this;
}

inline Dictionary& Dictionary::set(const char* key, const char* value)
{
    m_strings.set(key, value);
    return *this;
}

template <typename T>
inline Dictionary& Dictionary::set(const char* key, const T& value)
{
    return set(key, to_string(value).c_str());
}

template <>
inline Dictionary& Dictionary::set(const char* key, const Dictionary& value)
{
    m_dictionaries.set(key, value);
    return *this;
}

inline const char* Dictionary::get(const char* key) const
{
    return m_strings.get(key);
}

template <typename T>
inline T Dictionary::get(const char* key) const
{
    return m_strings.get<T>(key);
}

inline Dictionary& Dictionary::dictionary(const char* key)
{
    return m_dictionaries.get(key);
}

inline const Dictionary& Dictionary::dictionary(const char* key) const
{
    return m_dictionaries.get(key);
}

inline StringDictionary& Dictionary::strings()
{
    return m_strings;
}

inline const StringDictionary& Dictionary::strings() const
{
    return m_strings;
}

inline DictionaryDictionary& Dictionary::dictionaries()
{
    return m_dictionaries;
}

inline const DictionaryDictionary& Dictionary::dictionaries() const
{
    return m_dictionaries;
}

}   // namespace foundation
