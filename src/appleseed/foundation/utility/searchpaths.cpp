
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "searchpaths.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <vector>

using namespace boost;
using namespace std;

namespace foundation
{

//
// SearchPathsImpl class implementation.
//

struct SearchPathsImpl::Impl
{
    typedef vector<string> PathCollection;

    filesystem::path    m_root_path;
    PathCollection      m_paths;
};

SearchPathsImpl::SearchPathsImpl()
  : impl(new Impl())
{
}

SearchPathsImpl::~SearchPathsImpl()
{
    delete impl;
}

void SearchPathsImpl::clear()
{
    impl->m_root_path.clear();
    impl->m_paths.clear();
}

bool SearchPathsImpl::empty() const
{
    return impl->m_paths.empty();
}

size_t SearchPathsImpl::size() const
{
    return impl->m_paths.size();
}

bool SearchPathsImpl::has_root_path() const
{
    return !impl->m_root_path.empty();
}

const char* SearchPathsImpl::operator[](const size_t i) const
{
    assert(i < size());
    return impl->m_paths[i].c_str();
}

void SearchPathsImpl::do_set_root_path(const char* path)
{
    impl->m_root_path = filesystem::path(path).make_preferred();
}

char* SearchPathsImpl::do_get_root_path() const
{
    return duplicate_string(impl->m_root_path.string().c_str());
}

void SearchPathsImpl::do_push_back(const char* path)
{
    assert(path);
    impl->m_paths.push_back(path);
}

bool SearchPathsImpl::do_exist(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    if (!fp.is_absolute())
    {
        for (Impl::PathCollection::const_reverse_iterator
                i = impl->m_paths.rbegin(), e = impl->m_paths.rend(); i != e; ++i)
        {
            filesystem::path search_path(*i);

            if (has_root_path() && search_path.is_relative())
                search_path = impl->m_root_path / search_path;

            if (filesystem::exists(search_path / fp))
                return true;
        }

        if (has_root_path())
        {
            if (filesystem::exists(impl->m_root_path / fp))
                return true;
        }
    }

    return filesystem::exists(fp);
}

char* SearchPathsImpl::do_qualify(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    if (!fp.is_absolute())
    {
        for (Impl::PathCollection::const_reverse_iterator
                i = impl->m_paths.rbegin(), e = impl->m_paths.rend(); i != e; ++i)
        {
            filesystem::path search_path(*i);

            if (has_root_path() && search_path.is_relative())
                search_path = impl->m_root_path / search_path;

            filesystem::path qualified_fp = search_path / fp;

            if (filesystem::exists(qualified_fp))
            {
                qualified_fp.make_preferred();
                return duplicate_string(qualified_fp.string().c_str());
            }
        }

        if (has_root_path())
        {
            filesystem::path qualified_fp = impl->m_root_path / fp;

            if (filesystem::exists(qualified_fp))
            {
                qualified_fp.make_preferred();
                return duplicate_string(qualified_fp.string().c_str());
            }
        }
    }

    return duplicate_string(fp.string().c_str());
}

}   // namespace foundation
