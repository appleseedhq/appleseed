
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

    void push_back(const char *path)
    {
        filesystem::path p(path);        
        if (p.is_absolute())
        {
            m_paths.push_back(path);
            m_absolute_paths.push_back(path);
        }
        else
        {
            assert(!m_root.empty());
            filesystem::path abs_path = m_root / p;
            m_paths.push_back(path);
            m_absolute_paths.push_back(abs_path.string());
        }
    }
    
    filesystem::path m_root;
    PathCollection m_paths;
    PathCollection m_absolute_paths;
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
    impl->m_root.clear();
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

std::string SearchPaths::get_root_path() const
{
    if (impl->m_root.empty())
        return std::string();
    
    return impl->m_root.make_preferred().string();
}

void SearchPaths::set_root_path(const char* path)
{
    assert(impl->m_root.empty());
    assert(impl->m_paths.empty());
    
    filesystem::path p(path);
    assert(p.is_absolute());
    
    impl->m_root = p;
}

bool SearchPaths::has_root_path() const
{
    return !impl->m_root.empty();
}

const char* SearchPaths::operator[](const size_t i) const
{
    assert(i < size());
    return impl->m_paths[i].c_str();
}

void SearchPaths::push_back(const char* path)
{
    assert(path);

    impl->push_back(path);
}

bool SearchPaths::exist(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    if (fp.is_absolute())
        return filesystem::exists(fp);

    // first, test the root path, if it has been set.
    if (has_root_path())
    {
        if (filesystem::exists(impl->m_root / fp))
            return true;
    }
    
    // check all other paths.
    for (ConstReverseIterator i(abs_paths_rbegin()), e(abs_paths_rend()); i != e; ++i)
    {
        const filesystem::path qualified_fp = filesystem::path(*i) / fp;

        if (filesystem::exists(qualified_fp))
            return true;
    }

    return false;
}

char* SearchPaths::qualify(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);
    string result = fp.string();

    if (!fp.is_absolute())
    {
        // first, test the root path, if it has been set.
        if (has_root_path())
        {
            filesystem::path qualified_fp = impl->m_root / fp;
            if (filesystem::exists(qualified_fp))
            {
                result = qualified_fp.make_preferred().string();
                return duplicate_string(result.c_str());
            }
        }
        
        // check all other paths.
        for (ConstReverseIterator i(abs_paths_rbegin()), e(abs_paths_rend()); i != e; ++i)
        {
            filesystem::path qualified_fp = filesystem::path(*i) / fp;

            if (filesystem::exists(qualified_fp))
            {
                result = qualified_fp.make_preferred().string();
                break;
            }
        }
    }

    return duplicate_string(result.c_str());
}

SearchPaths::ConstIterator SearchPaths::begin() const
{
    return impl->m_paths.begin();
}

SearchPaths::ConstIterator SearchPaths::end() const
{
    return impl->m_paths.end();
}

SearchPaths::ConstIterator SearchPaths::abs_paths_begin() const
{
    return impl->m_absolute_paths.begin();    
}

SearchPaths::ConstIterator SearchPaths::abs_paths_end() const
{
    return impl->m_absolute_paths.end();    
}

SearchPaths::ConstReverseIterator SearchPaths::rbegin() const
{
    return impl->m_paths.rbegin();
}

SearchPaths::ConstReverseIterator SearchPaths::rend() const
{
    return impl->m_paths.rend();
}

SearchPaths::ConstReverseIterator SearchPaths::abs_paths_rbegin() const
{
    return impl->m_absolute_paths.rbegin();    
}

SearchPaths::ConstReverseIterator SearchPaths::abs_paths_rend() const
{
    return impl->m_absolute_paths.rend();    
}

}   // namespace foundation
