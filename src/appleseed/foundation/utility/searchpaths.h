
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/api/apistring.h"

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
// The paths are ordered by ascending priority:
// paths inserted later have precedence over those inserted earlier.
//

class APPLESEED_DLLSYMBOL SearchPaths
{
  public:
    // Return the default environment path separator for the platform.
    static const char environment_path_separator();

    // Return the path separator used by OSL and OpenImageIO.
    static const char osl_path_separator();

    // Constructor.
    SearchPaths();

    // Constructor.
    // Initializes search paths with the contents of the specified environment variable.
    SearchPaths(const char* envvar, const char separator);

    // Copy constructor.
    SearchPaths(const SearchPaths& other);

    // Destructor.
    ~SearchPaths();

    // Assignment.
    SearchPaths& operator=(const SearchPaths& other);

    // Swap.
    void swap(SearchPaths& other);

    // Set the root path used to resolve relative search paths.
    void set_root_path(const char* path);
    void set_root_path(const std::string& path);

    // Return the root path used to resolve relative search paths.
    APIString get_root_path() const;

    // Return true if the root path has been set.
    bool has_root_path() const;

    // Remove all search paths and clears the root path.
    void clear();

    // Remove all search paths except the root path and paths added from environment variables.
    void reset();

    // Return true if empty.
    bool empty() const;

    // Return the number of search paths.
    size_t size() const;

    // Return the i'th path.
    const char* operator[](const size_t i) const;

    // Insert a search path at the end of the collection.
    void push_back(const char* path);
    void push_back(const std::string& path);

    void split_and_push_back(const char* paths, const char separator);
    void split_and_push_back(const std::string& paths, const char separator);

    // Remove the i'th path.
    void remove(const size_t i);

    // Return true if a given file exists, that is, if the argument is the absolute
    // path to a file that exists, or it is the name of a file that exists in one of
    // the search paths.
    bool exist(const char* filepath) const;
    bool exist(const std::string& filepath) const;

    // Find a file in the search paths. If the file was found, the qualified path to
    // this file is returned. Otherwise the input path is returned.
    APIString qualify(const char* filepath) const;
    APIString qualify(const std::string& filepath) const;

    // Same as above but also returns the search path inside which the file was found.
    void qualify(const char* filepath, APIString* qualified_filepath, APIString* search_path) const;
    void qualify(const std::string& filepath, APIString* qualified_filepath, APIString* search_path) const;

    // Return a string with all the search paths separated by the specified separator.
    // The second variant returns the search paths in reverse order.
    APIString to_string(const char separator) const;
    APIString to_string_reversed(const char separator) const;

  protected:
    struct Impl;
    Impl* impl;

    APIString do_to_string(const char separator, const bool reversed) const;
};


//
// SearchPaths class implementation.
//

inline void SearchPaths::set_root_path(const std::string& path)
{
    set_root_path(path.c_str());
}

inline void SearchPaths::push_back(const std::string& path)
{
    push_back(path.c_str());
}

inline void SearchPaths::split_and_push_back(const std::string& paths, const char separator)
{
    split_and_push_back(paths.c_str(), separator);
}

inline bool SearchPaths::exist(const std::string& filepath) const
{
    return exist(filepath.c_str());
}

inline APIString SearchPaths::qualify(const std::string& filepath) const
{
    return qualify(filepath.c_str());
}

inline void SearchPaths::qualify(const std::string& filepath, APIString* qualified_filepath, APIString* search_path) const
{
    qualify(filepath.c_str(), qualified_filepath, search_path);
}

inline APIString SearchPaths::to_string(const char separator) const
{
    return do_to_string(separator, false);
}

inline APIString SearchPaths::to_string_reversed(const char separator) const
{
    return do_to_string(separator, true);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
