
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
#include "searchpaths.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iterator>
#include <vector>

namespace bf = boost::filesystem;

namespace foundation
{

//
// SearchPaths class implementation.
//

const char SearchPaths::environment_path_separator()
{
#if defined _WIN32
    return ';';
#else
    return ':';
#endif
}

const char SearchPaths::osl_path_separator()
{
    return ':';
}

struct SearchPaths::Impl
{
    typedef std::vector<std::string> PathCollection;

    bf::path        m_root_path;
    PathCollection  m_environment_paths;
    PathCollection  m_explicit_paths;
    PathCollection  m_all_paths;    // first all environment paths, then all explicit paths
};

SearchPaths::SearchPaths()
  : impl(new Impl())
{
}

SearchPaths::SearchPaths(const char* envvar, const char separator)
  : impl(new Impl())
{
    if (const char* value = getenv(envvar))
    {
        std::vector<std::string> paths;
        split(value, std::string(&separator, 1), paths);

        for (const std::string& path : paths)
        {
            // Ignore empty paths.
            if (path.empty())
                continue;

            // Ignore relative paths.
            if (!bf::path(path).is_absolute())
                continue;

            impl->m_environment_paths.emplace_back(path.c_str());
            impl->m_all_paths.emplace_back(path.c_str());
        }
    }
}

SearchPaths::SearchPaths(const SearchPaths& other)
  : impl(new Impl(*other.impl))
{
}

SearchPaths::~SearchPaths()
{
    delete impl;
}

SearchPaths& SearchPaths::operator=(const SearchPaths& other)
{
    SearchPaths tmp(other);
    swap(tmp);
    return *this;
}

void SearchPaths::swap(SearchPaths& other)
{
    std::swap(impl, other.impl);
}

void SearchPaths::set_root_path(const char* path)
{
    impl->m_root_path = bf::path(path).make_preferred();
}

APIString SearchPaths::get_root_path() const
{
    return APIString(impl->m_root_path.string().c_str());
}

bool SearchPaths::has_root_path() const
{
    return !impl->m_root_path.empty();
}

size_t SearchPaths::get_environment_path_count() const
{
    return impl->m_environment_paths.size();
}

const char* SearchPaths::get_environment_path(const size_t i) const
{
    assert(i < get_environment_path_count());
    return impl->m_environment_paths[i].c_str();
}

void SearchPaths::clear_explicit_paths()
{
    impl->m_explicit_paths.clear();
    impl->m_all_paths = impl->m_environment_paths;
}

size_t SearchPaths::get_explicit_path_count() const
{
    return impl->m_explicit_paths.size();
}

const char* SearchPaths::get_explicit_path(const size_t i) const
{
    assert(i < get_explicit_path_count());
    return impl->m_explicit_paths[i].c_str();
}

void SearchPaths::push_back_explicit_path(const char* path)
{
    assert(path);
    impl->m_explicit_paths.emplace_back(path);
    impl->m_all_paths.emplace_back(path);
}

void SearchPaths::remove_explicit_path(const size_t i)
{
    assert(i < get_explicit_path_count());
    impl->m_explicit_paths.erase(impl->m_explicit_paths.begin() + i);
}

size_t SearchPaths::get_path_count() const
{
    return impl->m_all_paths.size();
}

const char* SearchPaths::get_path(const size_t i) const
{
    assert(i < get_path_count());
    return impl->m_all_paths[i].c_str();
}

bool SearchPaths::exist(const char* filepath) const
{
    assert(filepath);

    const bf::path fp(filepath);

    if (!fp.is_absolute())
    {
        // Look in search paths.
        for (Impl::PathCollection::const_reverse_iterator
                i = impl->m_all_paths.rbegin(), e = impl->m_all_paths.rend(); i != e; ++i)
        {
            bf::path search_path(*i);

            // Make the search path absolute if there is a root path.
            if (has_root_path() && search_path.is_relative())
                search_path = impl->m_root_path / search_path;

            if (bf::exists(search_path / fp))
                return true;
        }

        // Look in the root path if there is one.
        if (has_root_path())
        {
            if (bf::exists(impl->m_root_path / fp))
                return true;
        }
    }

    return bf::exists(fp);
}

APIString SearchPaths::qualify(const char* filepath) const
{
    APIString qualified_filepath;
    qualify(filepath, &qualified_filepath, nullptr);
    return qualified_filepath;
}

void SearchPaths::qualify(const char* filepath, APIString* qualified_filepath_str, APIString* search_path_str) const
{
    assert(filepath);

    const bf::path fp(filepath);

    if (!fp.is_absolute())
    {
        // Look in search paths.
        for (Impl::PathCollection::const_reverse_iterator
                i = impl->m_all_paths.rbegin(), e = impl->m_all_paths.rend(); i != e; ++i)
        {
            bf::path search_path(*i);

            // Make the search path absolute if there is a root path.
            if (has_root_path() && search_path.is_relative())
                search_path = impl->m_root_path / search_path;

            bf::path qualified_fp = search_path / fp;

            if (bf::exists(qualified_fp))
            {
                qualified_fp.make_preferred();
                *qualified_filepath_str = APIString(qualified_fp.string().c_str());
                if (search_path_str)
                    *search_path_str = APIString(i->c_str());
                return;
            }
        }

        // Look in the root path if there is one.
        if (has_root_path())
        {
            bf::path qualified_fp = impl->m_root_path / fp;

            if (bf::exists(qualified_fp))
            {
                qualified_fp.make_preferred();
                *qualified_filepath_str = APIString(qualified_fp.string().c_str());
                if (search_path_str)
                    *search_path_str = APIString();
                return;
            }
        }
    }

    *qualified_filepath_str = APIString(fp.string().c_str());
    if (search_path_str)
        *search_path_str = APIString();
}

APIString SearchPaths::do_to_string(const char separator, const bool reversed) const
{
    Impl::PathCollection paths;

    const bool root_path = has_root_path();

    if (root_path)
        paths.emplace_back(impl->m_root_path.string().c_str());

    copy(impl->m_all_paths.begin(), impl->m_all_paths.end(), back_inserter(paths));

    if (reversed)
        reverse(paths.begin(), paths.end());

    std::string paths_str;

    for (size_t i = 0, e = paths.size(); i < e; ++i)
    {
        bf::path p(paths[i]);

        if (p.is_relative())
        {
            // Ignore relative paths if we don't have a root path.
            if (!root_path)
                continue;

            p = impl->m_root_path / p;
        }

        if (!paths_str.empty())
            paths_str.append(1, separator);

        paths_str.append(p.string());
    }

    return APIString(paths_str.c_str());
}

}   // namespace foundation
