
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

// Interface header.
#include "ptxcache.h"

// Standard headers.
#include <fstream>
#include <sstream>

using namespace std;
namespace bfs = boost::filesystem;

namespace foundation
{

//
// ExceptionCannotLoadPTX class implementation.
//

ExceptionCannotLoadPTX::ExceptionCannotLoadPTX(const char* path)
{
    string err("Cannot load PTX file ");
    err += path;
    err += ".";
    set_what(err.c_str());
}


//
// PTXCache class implementation.
//

PTXCache::PTXCache(const char* ptx_path)
  : m_ptx_path(ptx_path)
{
}

const string& PTXCache::get_ptx_code(const char* filename)
{
    map<string, string>::const_iterator it = m_ptx_code.find(filename);

    if (it == m_ptx_code.end())
    {
        bfs::path p(m_ptx_path / filename);
        const char* filepath = p.string().c_str();
        it = m_ptx_code.insert(make_pair(filename, read_source_file(filepath))).first;
    }

    return it->second;
}

string PTXCache::read_source_file(const char* filepath)
{
    ifstream file(filepath);

    if (file.good())
    {
        stringstream source_buffer;
        source_buffer << file.rdbuf();
        return source_buffer.str();
    }

    throw ExceptionCannotLoadPTX(filepath);
    return string();
}

}   // namespace renderer
