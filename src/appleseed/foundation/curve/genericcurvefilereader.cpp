
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Girish Ramesh, The appleseedhq Organization
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
#include "genericcurvefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/curve/binarycurvefilereader.h"
#include "foundation/curve/mitshairfilereader.h"
#include "foundation/string/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

namespace bf = boost::filesystem;

namespace foundation
{

struct GenericCurveFileReader::Impl
{
    std::string  m_filename;
    float        m_radius;
    size_t       m_degree;
};

GenericCurveFileReader::GenericCurveFileReader(const char* filename, const float radius, const size_t degree)
  : impl(new Impl())
{
    impl->m_filename = filename;
    impl->m_radius = radius;
    impl->m_degree = degree;
}

GenericCurveFileReader::~GenericCurveFileReader()
{
    delete impl;
}

void GenericCurveFileReader::read(ICurveBuilder& builder)
{
    const bf::path filepath(impl->m_filename);
    const std::string extension = lower_case(filepath.extension().string());

    if (extension == ".binarycurve")
    {
        BinaryCurveFileReader reader(impl->m_filename);
        reader.read(builder);
    }
    else if (extension == ".mitshair")
    {
        MitsHairFileReader reader(impl->m_filename, impl->m_radius, impl->m_degree);
        reader.read(builder);
    }
    else
    {
        throw ExceptionUnsupportedFileFormat(impl->m_filename.c_str());
    }
}

}   // namespace foundation
