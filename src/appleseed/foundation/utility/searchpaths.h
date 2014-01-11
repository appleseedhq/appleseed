
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

#ifndef APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
#define APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/string.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <string>

namespace foundation
{

//
// An ordered collection of search paths.
//
// The paths are ordered by ascending priority: paths inserted later have precedence
// over those inserted earlier).
//

class DLLSYMBOL SearchPaths 
  : public NonCopyable
{
  public:
    // Constructor.
    SearchPaths();

    // Destructor.
    ~SearchPaths();

    // Remove all search paths and clears the root path.
    void clear();

    // Return true if empty.
    bool empty() const;

    // Return the number of paths.
    size_t size() const;

    // Set the root path that is used to resolve relative paths.
    void set_root_path(const char* path);
    void set_root_path(const std::string& path);

    // Get the root path.
    std::string get_root_path() const;

    // Return true if the root path has been set.
    bool has_root_path() const;

    // Return the i'th path.
    const char* operator[](const size_t i) const;

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

  private:
    struct Impl;
    Impl* impl;

    static std::string make_string(const char* s);

    char* do_get_root_path() const;
    char* do_qualify(const char* filepath) const;
};


//
// SearchPaths class implementation.
//

inline std::string SearchPaths::make_string(const char* s)
{
    const std::string result = s;
    free_string(s);
    return result;
}

inline void SearchPaths::set_root_path(const std::string& path)
{
    set_root_path(path.c_str());
}

inline void SearchPaths::push_back(const std::string& path)
{
    return push_back(path.c_str());
}

inline std::string SearchPaths::get_root_path() const
{
    return make_string(do_get_root_path());
}

inline bool SearchPaths::exist(const std::string& filepath) const
{
    return exist(filepath.c_str());
}

inline std::string SearchPaths::qualify(const std::string& filepath) const
{
    return make_string(do_qualify(filepath.c_str()));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
