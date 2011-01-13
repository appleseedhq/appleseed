
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/utility/implptr.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// An ordered collection of search paths.
//
// The paths are ordered by descending priority: paths inserted earlier have precedence
// over those inserted later).
//

class FOUNDATIONDLL SearchPaths
  : public NonCopyable
{
  public:
    // Constructor.
    SearchPaths();

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
    PIMPL(SearchPaths);

    char* qualify(const char* filepath) const;
};


//
// SearchPaths class implementation.
//

inline void SearchPaths::push_back(const std::string& path)
{
    return push_back(path.c_str());
}

inline bool SearchPaths::exist(const std::string& filepath) const
{
    return exist(filepath.c_str());
}

inline std::string SearchPaths::qualify(const std::string& filepath) const
{
    const char* tmp = qualify(filepath.c_str());
    const std::string result = tmp;
    free_string(tmp);
    return result;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SEARCHPATHS_H
