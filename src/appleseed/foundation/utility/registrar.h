
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <map>
#include <string>
#include <utility>

namespace foundation
{

//
// A generic registrar.
//

template <typename T>
class Registrar
  : public NonCopyable
{
  public:
    typedef std::map<std::string, T*> Items;

    // Destructor.
    ~Registrar();

    // Remove all items.
    void clear();

    // Insert an item, replacing any existing item with the same name.
    void insert(const std::string& name, auto_release_ptr<T> item);

    // Lookup an item. Returns 0 if the item could not be found.
    T* lookup(const std::string& name) const;

    // Access the items directly.
    const Items& items() const;

  private:
    Items m_items;
};


//
// Registrar class implementation.
//

template <typename T>
Registrar<T>::~Registrar()
{
    clear();
}

template <typename T>
void Registrar<T>::clear()
{
    for (const_each<Items> i = m_items; i; ++i)
        i->second->release();

    m_items.clear();
}

template <typename T>
void Registrar<T>::insert(const std::string& name, auto_release_ptr<T> item)
{
    const typename Items::iterator i = m_items.find(name);

    if (i != m_items.end())
    {
        i->second->release();
        m_items.erase(i);
    }

    m_items.insert(std::make_pair(name, item.release()));
}

template <typename T>
T* Registrar<T>::lookup(const std::string& name) const
{
    const typename Items::const_iterator i = m_items.find(name);

    return i == m_items.end() ? nullptr : i->second;
}

template <typename T>
const typename Registrar<T>::Items& Registrar<T>::items() const
{
    return m_items;
}

}   // namespace foundation
