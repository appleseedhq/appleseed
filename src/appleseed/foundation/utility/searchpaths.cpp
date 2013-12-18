
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
#include "searchpaths.h"

// boost headers.
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
// SearchPaths class implementation.
//

struct SearchPaths::Impl
{
    typedef vector<string> PathCollection;

    filesystem::path    m_root_path;
    PathCollection      m_paths;
    PathCollection      m_absolute_paths;
};

SearchPaths::SearchPaths()
  : impl(new Impl())
{
}

SearchPaths::~SearchPaths()
{
    delete impl;
}

void SearchPaths::clear()
{
    impl->m_root_path.clear();
    impl->m_paths.clear();
    impl->m_absolute_paths.clear();
}

bool SearchPaths::empty() const
{
    return impl->m_paths.empty();
}

size_t SearchPaths::size() const
{
    return impl->m_paths.size();
}

void SearchPaths::set_root_path(const char* path)
{
    assert(impl->m_root_path.empty());
    assert(impl->m_paths.empty());

    impl->m_root_path = filesystem::path(path).make_preferred();
}

char* SearchPaths::do_get_root_path() const
{
    return duplicate_string(impl->m_root_path.string().c_str());
}

bool SearchPaths::has_root_path() const
{
    return !impl->m_root_path.empty();
}

const char* SearchPaths::operator[](const size_t i) const
{
    assert(i < size());
    return impl->m_paths[i].c_str();
}

void SearchPaths::push_back(const char* path)
{
    assert(path);

    const filesystem::path fp(path);

    if (fp.is_absolute())
    {
        impl->m_paths.push_back(path);
        impl->m_absolute_paths.push_back(path);
    }
    else
    {
        assert(!impl->m_root_path.empty());
        impl->m_paths.push_back(path);
        impl->m_absolute_paths.push_back((impl->m_root_path / fp).string());
    }
}

bool SearchPaths::exist(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    if (!fp.is_absolute())
    {
        for (const_reverse_iterator i(abs_paths_rbegin()), e(abs_paths_rend()); i != e; ++i)
        {
            if (filesystem::exists(filesystem::path(*i) / fp))
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

char* SearchPaths::do_qualify(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    if (!fp.is_absolute())
    {
        for (const_reverse_iterator i(abs_paths_rbegin()), e(abs_paths_rend()); i != e; ++i)
        {
            const filesystem::path qualified_fp = (filesystem::path(*i) / fp).make_preferred();

            if (filesystem::exists(qualified_fp))
                return duplicate_string(qualified_fp.string().c_str());
        }

        if (has_root_path())
        {
            const filesystem::path qualified_fp = (impl->m_root_path / fp).make_preferred();

            if (filesystem::exists(qualified_fp))
                return duplicate_string(qualified_fp.string().c_str());
        }
    }

    return duplicate_string(fp.string().c_str());
}

SearchPaths::const_iterator SearchPaths::begin() const
{
    return impl->m_paths.begin();
}

SearchPaths::const_iterator SearchPaths::end() const
{
    return impl->m_paths.end();
}

SearchPaths::const_iterator SearchPaths::abs_paths_begin() const
{
    return impl->m_absolute_paths.begin();    
}

SearchPaths::const_iterator SearchPaths::abs_paths_end() const
{
    return impl->m_absolute_paths.end();    
}

SearchPaths::const_reverse_iterator SearchPaths::rbegin() const
{
    return impl->m_paths.rbegin();
}

SearchPaths::const_reverse_iterator SearchPaths::rend() const
{
    return impl->m_paths.rend();
}

SearchPaths::const_reverse_iterator SearchPaths::abs_paths_rbegin() const
{
    return impl->m_absolute_paths.rbegin();    
}

SearchPaths::const_reverse_iterator SearchPaths::abs_paths_rend() const
{
    return impl->m_absolute_paths.rend();    
}

}   // namespace foundation
