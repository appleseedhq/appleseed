
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
// Terminology:
//
//   Root path
//     A path used to resolve relative search paths.
//
//   Environment search path
//     A search path extracted from an environment variable.
//
//   Explicit search path
//     A search path explicitly and manually added to the collection.
//
// Priorities:
//
//   Paths are ordered by ascending priority: paths inserted later have precedence
//   over those inserted earlier.
//
//   Environment search paths all have lower priority than explicit search paths.
//

class APPLESEED_DLLSYMBOL SearchPaths
{
  public:
    // Return the default environment path separator for the platform.
    static const char environment_path_separator();

    // Return the path separator used by OSL and OpenImageIO.
    static const char osl_path_separator();

    // Constructor.
    // No root path is set and the search path collection is empty.
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

    //
    // Root path.
    //

    // Set the root path.
    void set_root_path(const char* path);
    void set_root_path(const std::string& path);

    // Return the root path.
    APIString get_root_path() const;

    // Return true if the root path has been set.
    bool has_root_path() const;

    //
    // Environment search paths.
    //

    // Return the number of environment search paths.
    size_t get_environment_path_count() const;

    // Return the i'th environment search path.
    const char* get_environment_path(const size_t i) const;

    //
    // Explicit search paths.
    //

    // Remove all explicit search paths.
    void clear_explicit_paths();

    // Return the number of explicit search paths.
    size_t get_explicit_path_count() const;

    // Return the i'th explicit search path.
    const char* get_explicit_path(const size_t i) const;

    // Insert an explicit search path at the end of the collection.
    void push_back_explicit_path(const char* path);
    void push_back_explicit_path(const std::string& path);

    // Remove the i'th explicit search path.
    void remove_explicit_path(const size_t i);

    //
    // Combined search paths.
    //

    // Return the total number of search paths (environment paths and explicit paths).
    size_t get_path_count() const;

    // Return the i'th path from the collection of environment paths and explicit paths.
    const char* get_path(const size_t i) const;

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

    // Return a string with all search paths separated by the specified separator.
    // The second variant returns search paths in reverse order.
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

inline void SearchPaths::push_back_explicit_path(const std::string& path)
{
    push_back_explicit_path(path.c_str());
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

}   // namespace foundation
