
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "itemregistry.h"

// Standard headers.
#include <cassert>
#include <utility>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

void ItemRegistry::insert(
    const UniqueID  uid,
    ItemBase*       item)
{
    m_mutex.lock();

#ifndef NDEBUG
    const pair<RegistryType::iterator, bool> result =
#endif
    m_registry.insert(make_pair(uid, item));

    assert(result.second);

    m_mutex.unlock();
}

void ItemRegistry::remove(const UniqueID uid)
{
    m_mutex.lock();

#ifndef NDEBUG
    const RegistryType::size_type result =
#endif
    m_registry.erase(uid);

    assert(result == 1);

    m_mutex.unlock();
}

ItemBase* ItemRegistry::get_item(const UniqueID uid) const
{
    m_mutex.lock();

    const RegistryType::const_iterator i = m_registry.find(uid);
    ItemBase* result = i == m_registry.end() ? 0 : i->second;

    m_mutex.unlock();

    return result;
}

}   // namespace studio
}   // namespace appleseed
