
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

#pragma once

// appleseed.foundation headers.
#include "foundation/curve/icurvefilewriter.h"
#include "foundation/utility/bufferedfile.h"

// Standard headers.
#include <cstdint>
#include <string>

// Forward declarations.
namespace foundation    { class ICurveWalker; }

namespace foundation
{

//
// Writer for a simple binary curve file format.
//

class BinaryCurveFileWriter
  : public ICurveFileWriter
{
  public:
    // Constructor.
    explicit BinaryCurveFileWriter(const std::string& filename);

    // Write a curve object.
    void write(const ICurveWalker& walker) override;

  private:
    const std::string           m_filename;
    BufferedFile                m_file;
    LZ4CompressedWriterAdapter  m_writer;

    void write_signature();
    void write_version();

    void write_curves(const ICurveWalker& walker);
    void write_curve_count(const ICurveWalker& walker);
    void write_basis(const ICurveWalker& walker);
    void write_curve(const ICurveWalker& walker, const std::uint32_t curve_id, std::uint32_t& vertex_count);
};

}   // namespace foundation
