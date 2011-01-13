
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

#ifndef APPLESEED_FOUNDATION_UTILITY_CONTAINERS_ANYARRAY_H
#define APPLESEED_FOUNDATION_UTILITY_CONTAINERS_ANYARRAY_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/pagedarray.h"
#include "foundation/utility/numerictype.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <map>
#include <string>

namespace foundation
{

//
// Unordered collection of typed and named elements.
//

class AnyArray
{
  private:
    // Entry in the table of contents.
    struct Info
    {
        NumericTypeID   m_type;
        size_t          m_index;
    };

    // Name -> Info map.
    typedef std::map<std::string, Info> NameToInfoMap;

  public:
    // Exception thrown when attempting to insert an element with the same name as an
    // other element in the array.
    struct ExceptionDuplicateName : public Exception {};

    // Exception thrown when attempting to access an element which is not in the array.
    struct ExceptionUnknownName : public Exception {};

    // Exception thrown when attempting to access an element with a different type than
    // its declaration type.
    struct ExceptionTypeMismatch : public Exception {};

    // Constant array iterator.
    class ConstIterator
    {
      public:
        // Copy constructor.
        ConstIterator(const ConstIterator& rhs);

        // Prefix and postfix increment and decrement operators.
        ConstIterator& operator++();                // prefix increment
        ConstIterator  operator++(int) const;       // postfix increment
        ConstIterator& operator--();                // prefix decrement
        ConstIterator  operator--(int) const;       // postfix decrement

        // Comparison operators.
        bool operator==(const ConstIterator& rhs) const;
        bool operator!=(const ConstIterator& rhs) const;

        // Return the name of the pointed element.
        std::string name() const;

        // Return the type ID of the pointed element.
        NumericTypeID type() const;

        // Return the value of the pointed element.
        template <typename T>
        const T& value() const;

      private:
        friend class AnyArray;

        const AnyArray&                 m_array;    // parent array
        NameToInfoMap::const_iterator   m_toc_it;   // iterator in the table of contents

        // Initializing constructor.
        ConstIterator(
            const AnyArray&                         array,
            const NameToInfoMap::const_iterator&    toc_it);
    };

    // Constructors.
    AnyArray();
    AnyArray(const AnyArray& rhs);

    // Assignment operator.
    AnyArray& operator=(const AnyArray& rhs);

    // Remove all elements from the array.
    void clear();

    // Return true if the array is empty.
    bool empty() const;

    // Return the size of the array.
    size_t size() const;

    // Insert a new typed and named element into the array. It is always invalid
    // to insert an element with the same name as an element already present in
    // the array. If attempted, an exception is thrown. The returned references
    // remain valid until the AnyArray object is destructed.
    template <typename T>
    T& insert(const std::string& name, const T& value = T());

    // Checked, mutable access to the value of a given element. The returned
    // references remain valid until the AnyArray object is destructed.
    template <typename T>
    T& at(const std::string& name);

    // Checked, constant access to the value of a given element. The returned
    // references remain valid until the AnyArray object is destructed.
    template <typename T>
    const T& at(const std::string& name) const;

    // Return a constant iterator pointing to the first element.
    ConstIterator begin() const;

    // Return a constant iterator pointing one entry beyond the last element.
    ConstIterator end() const;

    // Return a constant iterator pointing to a given element.
    ConstIterator find(const std::string& name) const;

  private:
    friend class ConstIterator;

    // Table of contents, maps a name to an index into the appropriate array.
    NameToInfoMap           m_toc;

    // Define the ValueArray type as a partial specialization of foundation::PagedArray.
    template <typename T>
    struct ValueArray : public PagedArray<T, 16> {};

    // Arrays of values.
    ValueArray<int8>        m_int8_values;
    ValueArray<int16>       m_int16_values;
    ValueArray<int32>       m_int32_values;
    ValueArray<int64>       m_int64_values;
    ValueArray<uint8>       m_uint8_values;
    ValueArray<uint16>      m_uint16_values;
    ValueArray<uint32>      m_uint32_values;
    ValueArray<uint64>      m_uint64_values;
    ValueArray<float>       m_float_values;
    ValueArray<double>      m_double_values;

    // Return a constant iterator to the info of a given element.
    // Throws an ExceptionUnknownName exception if the element does not exist.
    NameToInfoMap::const_iterator find_info(
        const std::string&  name) const;

    // Helper function returning a mutable reference to the array corresponding to a given type.
    template <typename T> ValueArray<T>& get_array();

    // Helper function returning a constant reference to the array corresponding to a given type.
    template <typename T> const ValueArray<T>& get_array() const;
};


//
// AnyArray class implementation.
//

// Constructors.
inline AnyArray::AnyArray()
{
}
inline AnyArray::AnyArray(const AnyArray& rhs)
  : m_toc          (rhs.m_toc)
  , m_int8_values  (rhs.m_int8_values)
  , m_int16_values (rhs.m_int16_values)
  , m_int32_values (rhs.m_int32_values)
  , m_int64_values (rhs.m_int64_values)
  , m_uint8_values (rhs.m_uint8_values)
  , m_uint16_values(rhs.m_uint16_values)
  , m_uint32_values(rhs.m_uint32_values)
  , m_uint64_values(rhs.m_uint64_values)
  , m_float_values (rhs.m_float_values)
  , m_double_values(rhs.m_double_values)
{
}

// Assignment operator.
inline AnyArray& AnyArray::operator=(const AnyArray& rhs)
{
    m_toc           = rhs.m_toc;
    m_int8_values   = rhs.m_int8_values;
    m_int16_values  = rhs.m_int16_values;
    m_int32_values  = rhs.m_int32_values;
    m_int64_values  = rhs.m_int64_values;
    m_uint8_values  = rhs.m_uint8_values;
    m_uint16_values = rhs.m_uint16_values;
    m_uint32_values = rhs.m_uint32_values;
    m_uint64_values = rhs.m_uint64_values;
    m_float_values  = rhs.m_float_values;
    m_double_values = rhs.m_double_values;
    return *this;
}

// Remove all elements from the array.
inline void AnyArray::clear()
{
    m_toc.clear();
    m_int8_values.clear();
    m_int16_values.clear();
    m_int32_values.clear();
    m_int64_values.clear();
    m_uint8_values.clear();
    m_uint16_values.clear();
    m_uint32_values.clear();
    m_uint64_values.clear();
    m_float_values.clear();
    m_double_values.clear();
}

// Return true if the array is empty.
inline bool AnyArray::empty() const
{
    return m_toc.empty();
}

// Return the size of the array.
inline size_t AnyArray::size() const
{
    return m_toc.size();
}

// Insert a new typed and named element into the array.
template <typename T>
T& AnyArray::insert(const std::string& name, const T& value)
{
    // Throw an exception if an element of this name already exist.
    if (m_toc.find(name) != m_toc.end())
        throw ExceptionDuplicateName();

    // Retrieve the proper array of values.
    ValueArray<T>& array = get_array<T>();

    // Insert the new element.
    const size_t index = array.size();
    array.push_back(value);

    // Create an entry for this element into the table of contents.
    Info& info = m_toc[name];
    info.m_type = NumericType::id<T>();
    info.m_index = index;

    // Return a mutable reference to the new element.
    return array[index];
}

// Checked, mutable access to the value of a given element.
template <typename T>
T& AnyArray::at(const std::string& name)
{
    // Find the element. Throw an exception if the element does not exist.
    const NameToInfoMap::const_iterator it = find_info(name);

    // Throw an exception if the type of the element does not match the expected type.
    if (it->second.m_type != NumericType::id<T>())
        throw ExceptionTypeMismatch();

    // Return a mutable reference to the element.
    return get_array<T>()[it->second.m_index];
}

// Checked, constant access to the value of a given element.
template <typename T>
const T& AnyArray::at(const std::string& name) const
{
    // Find the element. Throw an exception if the element does not exist.
    const NameToInfoMap::const_iterator it = find_info(name);

    // Throw an exception if the type of the element does not match the expected type.
    if (it->second.m_type != NumericType::id<T>())
        throw ExceptionTypeMismatch();

    // Return mutable reference to element.
    return get_array<T>()[it->second.m_index];
}

// Return a constant iterator pointing to the first element.
inline AnyArray::ConstIterator AnyArray::begin() const
{
    return ConstIterator(*this, m_toc.begin());
}

// Return a constant iterator pointing one entry beyond the last element.
inline AnyArray::ConstIterator AnyArray::end() const
{
    // Don't check for emptiness first.
    return ConstIterator(*this, m_toc.end());
}

// Return a constant iterator pointing to a given element.
inline AnyArray::ConstIterator AnyArray::find(
    const std::string&  name) const
{
    return ConstIterator(*this, m_toc.find(name));
}

// Return a constant iterator to the info of a given element.
// Throws an ExceptionUnknownName exception if the element does not exist.
inline AnyArray::NameToInfoMap::const_iterator AnyArray::find_info(
    const std::string&  name) const
{
    const NameToInfoMap::const_iterator it = m_toc.find(name);
    if (it == m_toc.end())
        throw ExceptionUnknownName();
    return it;
}

// Helper function returning a mutable reference to the array corresponding to a given type.
template <> inline AnyArray::ValueArray<int8>& AnyArray::get_array()                        { return m_int8_values;   }
template <> inline AnyArray::ValueArray<int16>& AnyArray::get_array()                       { return m_int16_values;  }
template <> inline AnyArray::ValueArray<int32>& AnyArray::get_array()                       { return m_int32_values;  }
template <> inline AnyArray::ValueArray<int64>& AnyArray::get_array()                       { return m_int64_values;  }
template <> inline AnyArray::ValueArray<uint8>& AnyArray::get_array()                       { return m_uint8_values;  }
template <> inline AnyArray::ValueArray<uint16>& AnyArray::get_array()                      { return m_uint16_values; }
template <> inline AnyArray::ValueArray<uint32>& AnyArray::get_array()                      { return m_uint32_values; }
template <> inline AnyArray::ValueArray<uint64>& AnyArray::get_array()                      { return m_uint64_values; }
template <> inline AnyArray::ValueArray<float>& AnyArray::get_array()                       { return m_float_values;  }
template <> inline AnyArray::ValueArray<double>& AnyArray::get_array()                      { return m_double_values; }

// Helper function returning a constant reference to the array corresponding to a given type.
template <> inline const AnyArray::ValueArray<int8>& AnyArray::get_array() const            { return m_int8_values;   }
template <> inline const AnyArray::ValueArray<int16>& AnyArray::get_array() const           { return m_int16_values;  }
template <> inline const AnyArray::ValueArray<int32>& AnyArray::get_array() const           { return m_int32_values;  }
template <> inline const AnyArray::ValueArray<int64>& AnyArray::get_array() const           { return m_int64_values;  }
template <> inline const AnyArray::ValueArray<uint8>& AnyArray::get_array() const           { return m_uint8_values;  }
template <> inline const AnyArray::ValueArray<uint16>& AnyArray::get_array() const          { return m_uint16_values; }
template <> inline const AnyArray::ValueArray<uint32>& AnyArray::get_array() const          { return m_uint32_values; }
template <> inline const AnyArray::ValueArray<uint64>& AnyArray::get_array() const          { return m_uint64_values; }
template <> inline const AnyArray::ValueArray<float>& AnyArray::get_array() const           { return m_float_values;  }
template <> inline const AnyArray::ValueArray<double>& AnyArray::get_array() const          { return m_double_values; }


//
// AnyArray::ConstIterator class implementation.
//

// Initializing constructor.
inline AnyArray::ConstIterator::ConstIterator(
    const AnyArray&                         array,
    const NameToInfoMap::const_iterator&    toc_it)
  : m_array(array)
  , m_toc_it(toc_it)
{
}

// Copy constructor.
inline AnyArray::ConstIterator::ConstIterator(const ConstIterator& rhs)
  : m_array(rhs.m_array)
  , m_toc_it(rhs.m_toc_it)
{
}

// Prefix and postfix increment and decrement operators.

inline AnyArray::ConstIterator&
AnyArray::ConstIterator::operator++()               // prefix increment
{
    ++m_toc_it;
    return *this;
}

inline AnyArray::ConstIterator
AnyArray::ConstIterator::operator++(int) const      // postfix increment
{
    NameToInfoMap::const_iterator it = m_toc_it;
    return ConstIterator(m_array, ++it);
}

inline AnyArray::ConstIterator&
AnyArray::ConstIterator::operator--()               // prefix decrement
{
    --m_toc_it;
    return *this;
}

inline AnyArray::ConstIterator
AnyArray::ConstIterator::operator--(int) const      // postfix decrement
{
    NameToInfoMap::const_iterator it = m_toc_it;
    return ConstIterator(m_array, --it);
}

// Comparison operators.

inline bool AnyArray::ConstIterator::operator==(const ConstIterator& rhs) const
{
    assert(&m_array == &rhs.m_array);
    return m_toc_it == rhs.m_toc_it;
}

inline bool AnyArray::ConstIterator::operator!=(const ConstIterator& rhs) const
{
    assert(&m_array == &rhs.m_array);
    return m_toc_it != rhs.m_toc_it;
}

// Return the name of the pointed element.
inline std::string AnyArray::ConstIterator::name() const
{
    return m_toc_it->first;
}

// Return the type ID of the pointed element.
inline NumericTypeID AnyArray::ConstIterator::type() const
{
    return m_toc_it->second.m_type;
}

// Return the value of the pointed element.
template <typename T>
inline const T& AnyArray::ConstIterator::value() const
{
    const Info& info = m_toc_it->second;
    return m_array.get_array<T>()[info.m_index];
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_CONTAINERS_ANYARRAY_H
