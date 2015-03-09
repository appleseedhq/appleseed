
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
#define APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/string.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace foundation
{

//
// An ordered collection of search paths.
//
// The paths are ordered by ascending priority: paths inserted later have precedence
// over those inserted earlier).
//

class APPLESEED_DLLSYMBOL SearchPathsImpl
  : public NonCopyable
{
  public:
    // Constructor.
    SearchPathsImpl();

    // Constructor.
    explicit SearchPathsImpl(const char* envvar);

    // Destructor.
    ~SearchPathsImpl();

    // Remove all search paths and clears the root path.
    void clear();

    // Return true if empty.
    bool empty() const;

    // Return the number of paths.
    size_t size() const;

    // Return true if the root path has been set.
    bool has_root_path() const;

    // Return the i'th path.
    const char* operator[](const size_t i) const;

  protected:
    struct Impl;
    Impl* impl;

    void do_set_root_path(const char* path);
    char* do_get_root_path() const;
    void do_push_back(const char* path);
    bool do_exist(const char* filepath) const;
    char* do_qualify(const char* filepath) const;
    char* do_to_string(const char separator, const bool reversed) const;
};

class SearchPaths
  : public SearchPathsImpl
{
  public:
    // Constructor.
    SearchPaths();

    // Constructor. Initializes search paths with the contents of the specified
    // environment variable.
    explicit SearchPaths(const char* envvar);

    // Set the root path that is used to resolve relative paths.
    void set_root_path(const char* path);
    void set_root_path(const std::string& path);

    // Return the root path that is used to resolve relative paths.
    std::string get_root_path() const;

    // Insert a search path at the end of the collection.
    void push_back(const char* path);
    void push_back(const std::string& path);

    // Return true if a given file exists, that is, if the argument is the absolute
    // path to a file that exists, or it is the name of a file that exists in one of
    // the search paths.
    bool exist(const char* filepath) const;
    bool exist(const std::string& filepath) const;

    // Find a file in the search paths. If the file was found, the qualified path to
    // this file is returned. Otherwise the input path is returned.
    std::string qualify(const std::string& filepath) const;

    // Return a string with all the search paths separated by the specified separator,
    // optionally making them absolute and/or listing them in reverse order.
    std::string to_string(
        const char separator = ':',
        const bool reversed = false) const;
};


//
// SearchPaths class implementation.
//

inline SearchPaths::SearchPaths()
{
}

inline SearchPaths::SearchPaths(const char* envvar)
  : SearchPathsImpl(envvar)
{
}

inline void SearchPaths::set_root_path(const char* path)
{
    do_set_root_path(path);
}

inline void SearchPaths::set_root_path(const std::string& path)
{
    do_set_root_path(path.c_str());
}

inline void SearchPaths::push_back(const char* path)
{
    do_push_back(path);
}

inline void SearchPaths::push_back(const std::string& path)
{
    do_push_back(path.c_str());
}

inline std::string SearchPaths::get_root_path() const
{
    return convert_to_std_string(do_get_root_path());
}

inline bool SearchPaths::exist(const char* filepath) const
{
    return do_exist(filepath);
}

inline bool SearchPaths::exist(const std::string& filepath) const
{
    return do_exist(filepath.c_str());
}

inline std::string SearchPaths::qualify(const std::string& filepath) const
{
    return convert_to_std_string(do_qualify(filepath.c_str()));
}

inline std::string SearchPaths::to_string(
    const char separator,
    const bool reversed) const
{
    return convert_to_std_string(do_to_string(separator, reversed));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
