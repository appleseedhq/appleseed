
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_DISPATCHER_H
#define APPLESEED_FOUNDATION_UTILITY_DISPATCHER_H

// appleseed.foundation headers.
#include "foundation/core/concepts.h"

// Standard headers.
#include <map>
#include <string>
#include <utility>

namespace foundation
{

//
// A function dispatcher, allows to call functions by name.
//

template <typename FunctionPtr>
class Dispatcher
  : public NonCopyable
{
  public:
    // Declare a new named function.
    // Return false if a function was already declared with this name.
    bool declare(
        const std::string&  name,
        FunctionPtr         function);

    // Lookup a function by name.
    // Return 0 if the function could not be found.
    FunctionPtr lookup(
        const std::string&  name) const;

  private:
    typedef std::map<std::string, FunctionPtr> FunctionMap;
    FunctionMap m_function_map;
};


//
// Dispatcher class implementation.
//

// Declare a new named function.
template <typename FunctionPtr>
bool Dispatcher<FunctionPtr>::declare(
    const std::string&      name,
    FunctionPtr             function)
{
    return m_function_map.insert(std::make_pair(name, function)).second;
}

// Lookup a function by name.
template <typename FunctionPtr>
FunctionPtr Dispatcher<FunctionPtr>::lookup(
    const std::string&      name) const
{
    const typename FunctionMap::const_iterator i = m_function_map.find(name);
    return i == m_function_map.end() ? 0 : i->second;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_DISPATCHER_H
