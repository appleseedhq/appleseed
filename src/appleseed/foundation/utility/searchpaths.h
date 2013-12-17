
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

    // Remove all search paths.
    void clear();

    // Returns true if empty.
    bool empty() const;

    // Returns the number of paths.
    size_t size() const;

    // Gets the root path, the path used to resolve relative paths in the searchpaths
    std::string get_root_path() const;

    // Sets the root path, the path used to resolve relative paths in the searchpaths.
    void set_root_path(const char* path);
    template<typename T> void set_root_path(const std::basic_string<T>& path);

    // Returns true if the root path has been set.
    bool has_root_path() const;
    
    // Returns the ith path.
    const char* operator[](const size_t i) const;

    // Insert a search path at the end of the collection.
    void push_back(const char* path);
    template <typename T> void push_back(const std::basic_string<T>& path);

    // Return true if a given file exists, that is, if the argument is the absolute
    // path to a file that exists, or it is the name of a file that exists in one of
    // the search paths.
    bool exist(const char* filepath) const;
    template <typename T> bool exist(const std::basic_string<T>& filepath) const;

    // Find a file in the search paths. If the file was found, the qualified path to
    // this file is returned. Otherwise the input path is returned.
    template <typename T> std::basic_string<T> qualify(const std::basic_string<T>& filepath) const;

    // Const iterator.
    typedef std::vector<std::string>::const_iterator ConstIterator;

    ConstIterator begin() const;
    ConstIterator end() const;
    
    ConstIterator abs_paths_begin() const;
    ConstIterator abs_paths_end() const;

    // Const reverse iterator.
    typedef std::vector<std::string>::const_reverse_iterator ConstReverseIterator;

    ConstReverseIterator rbegin() const;
    ConstReverseIterator rend() const;
    
    ConstReverseIterator abs_paths_rbegin() const;
    ConstReverseIterator abs_paths_rend() const;
    
  private:
    struct Impl;
    Impl* impl;

    char* qualify(const char* filepath) const;
};


//
// SearchPaths class implementation.
//

template<typename T> 
inline void SearchPaths::set_root_path(const std::basic_string<T>& path)
{
    set_root_path(path.c_str());
}

template <typename T>
inline void SearchPaths::push_back(const std::basic_string<T>& path)
{
    return push_back(path.c_str());
}

template <typename T>
inline bool SearchPaths::exist(const std::basic_string<T>& filepath) const
{
    return exist(filepath.c_str());
}

template <typename T>
inline std::basic_string<T> SearchPaths::qualify(const std::basic_string<T>& filepath) const
{
    const char* tmp = qualify(filepath.c_str());
    const std::basic_string<T> result = tmp;
    free_string(tmp);
    return result;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
