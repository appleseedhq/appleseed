
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_CUDA_PTXCACHE_H
#define APPLESEED_FOUNDATION_CUDA_PTXCACHE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exception.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <map>
#include <string>

namespace foundation
{

//
// Exception thrown when a PTX file cannot be loaded.
//

class ExceptionCannotLoadPTX
  : public foundation::Exception
{
  public:
    // Constructor.
    explicit ExceptionCannotLoadPTX(const char* path);
};


//
// PTX Cache.
//

class PTXCache
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit PTXCache(const char* ptx_dir);

    const std::string& get_ptx_code(const char* filename);

  private:
    boost::filesystem::path             m_ptx_path;
    std::map<std::string, std::string>  m_ptx_code;

    static std::string read_source_file(const char* filepath);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_CUDA_PTXCACHE_H
