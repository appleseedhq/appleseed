
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
#include "genericmeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/meshio/binarymeshfilereader.h"
#include "foundation/meshio/objmeshfilereader.h"
#include "foundation/string/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

namespace bf = boost::filesystem;

namespace foundation
{

struct GenericMeshFileReader::Impl
{
    std::string  m_filename;
    int          m_obj_options;
};

GenericMeshFileReader::GenericMeshFileReader(const char* filename)
  : impl(new Impl())
{
    impl->m_filename = filename;
    impl->m_obj_options = OBJMeshFileReader::Default;
}

GenericMeshFileReader::~GenericMeshFileReader()
{
    delete impl;
}

int GenericMeshFileReader::get_obj_options() const
{
    return impl->m_obj_options;
}

void GenericMeshFileReader::set_obj_options(const int obj_options)
{
    impl->m_obj_options = obj_options;
}

void GenericMeshFileReader::read(IMeshBuilder& builder)
{
    const bf::path filepath(impl->m_filename);
    const std::string extension = lower_case(filepath.extension().string());

    if (extension == ".obj")
    {
        OBJMeshFileReader reader(impl->m_filename, impl->m_obj_options);
        reader.read(builder);
    }
    else if (extension == ".binarymesh")
    {
        BinaryMeshFileReader reader(impl->m_filename);
        reader.read(builder);
    }
    else
    {
        throw ExceptionUnsupportedFileFormat(impl->m_filename.c_str());
    }
}

}   // namespace foundation
