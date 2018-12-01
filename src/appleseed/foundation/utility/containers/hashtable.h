
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
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <utility>
#include <vector>

namespace foundation
{

//
// A minimalist hash table implementation with many restrictions:
//
//   * No dynamic resizing (the table is cleared on resize)
//   * Table size must be a power of two
//   * No deletion of elements
//   * No control over memory allocation
//

template <typename KeyType, typename KeyHasherType, typename ValueType>
class HashTable
{
  public:
    // Constructor, creates an empty hash table with a given key hasher.
    explicit HashTable(const KeyHasherType& key_hasher);

    // Destructor.
    ~HashTable();

    // Resize the table. The size must be a power of two.
    // All previously inserted elements are lost.
    void resize(const size_t size);

    // Insert an element into the hash table. The key must be unique.
    void insert(const KeyType& key, const ValueType& value);

    // Retrieve an element from the hash table. Returns nullptr if the element cannot be found.
    const ValueType* get(const KeyType& key) const;

  private:
    typedef std::pair<KeyType, ValueType> Entry;
    typedef std::vector<Entry> EntryVector;

    const KeyHasherType&    m_key_hasher;
    size_t                  m_mask;
    EntryVector*            m_vectors;
};


//
// HashTable class implementation.
//

template <typename KeyType, typename KeyHasherType, typename ValueType>
HashTable<KeyType, KeyHasherType, ValueType>::HashTable(const KeyHasherType& key_hasher)
  : m_key_hasher(key_hasher)
  , m_mask(0)
  , m_vectors(new EntryVector[1])
{
}

template <typename KeyType, typename KeyHasherType, typename ValueType>
HashTable<KeyType, KeyHasherType, ValueType>::~HashTable()
{
    delete[] m_vectors;
}

template <typename KeyType, typename KeyHasherType, typename ValueType>
void HashTable<KeyType, KeyHasherType, ValueType>::resize(const size_t size)
{
    assert(size == 0 || is_pow2(size));

    delete[] m_vectors;

    m_mask = size > 0 ? size - 1 : 0;
    m_vectors = size > 0 ? new EntryVector[size] : new EntryVector[1];
}

template <typename KeyType, typename KeyHasherType, typename ValueType>
inline void HashTable<KeyType, KeyHasherType, ValueType>::insert(const KeyType& key, const ValueType& value)
{
    assert(m_vectors);

    const size_t index = m_key_hasher(key) & m_mask;
    EntryVector& vec = m_vectors[index];

    vec.push_back(std::make_pair(key, value));
}

template <typename KeyType, typename KeyHasherType, typename ValueType>
inline const ValueType* HashTable<KeyType, KeyHasherType, ValueType>::get(const KeyType& key) const
{
    assert(m_vectors);

    const size_t index = m_key_hasher(key) & m_mask;
    const EntryVector& vec = m_vectors[index];

    for (size_t i = 0, e = vec.size(); i < e; ++i)
    {
        if (vec[i].first == key)
            return &vec[i].second;
    }

    return nullptr;
}

}   // namespace foundation
