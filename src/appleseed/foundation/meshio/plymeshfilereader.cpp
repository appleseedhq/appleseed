
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
#include "plymeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/memory/memory.h"
#include "foundation/meshio/imeshbuilder.h"

// Standard headers.
#include <cstring>
#include <map>
#include <utility>
#include <vector>
#include <iostream>

namespace foundation
{
    struct PLYMeshFileReader::Impl
    {
        const int       m_options;
        IMeshBuilder&   m_builder;

        Impl(
            const int options,
            IMeshBuilder& builder)
        : m_options(options)
        , m_builder(builder)
        {
        }

    };

    PLYMeshFileReader::PLYMeshFileReader(
        const std::string&  filename,
        const int           options
    )
    : m_filename(filename)
    , m_options(options)
    {
    }

    void PLYMeshFileReader::read(IMeshBuilder& builder)
    {
        Impl impl(m_options, builder);

        std::cout << "PLY mesh file reader should parse the file now." << std::endl;
    }

    } // namespace foundation
