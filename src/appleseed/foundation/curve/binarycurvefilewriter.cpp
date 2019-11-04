
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
#include "binarycurvefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/curve/icurvewalker.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstring>

namespace foundation
{

//
// BinaryCurveFileWriter class implementation.
//

BinaryCurveFileWriter::BinaryCurveFileWriter(const std::string& filename)
  : m_filename(filename)
  , m_writer(m_file, 256 * 1024)
{
}

void BinaryCurveFileWriter::write(const ICurveWalker& walker)
{
    if (!m_file.is_open())
    {
        m_file.open(
            m_filename.c_str(),
            BufferedFile::BinaryType,
            BufferedFile::WriteMode);

        if (!m_file.is_open())
            throw ExceptionIOError();

        write_signature();
        write_version();
    }

    write_curves(walker);
}

void BinaryCurveFileWriter::write_signature()
{
    static const char Signature[11] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'C', 'U', 'R', 'V', 'E' };
    checked_write(m_file, Signature, sizeof(Signature));
}

void BinaryCurveFileWriter::write_version()
{
    const std::uint16_t Version = 2;
    checked_write(m_file, Version);
}

void BinaryCurveFileWriter::write_curves(const ICurveWalker& walker)
{
    write_basis(walker);
    write_curve_count(walker);

    std::uint32_t vertex_count = 0;

    for (std::uint32_t i = 0; i < walker.get_curve_count(); ++i)
        write_curve(walker, i, vertex_count);
}

void BinaryCurveFileWriter::write_basis(const ICurveWalker& walker)
{
    const unsigned char basis = static_cast<unsigned char>(walker.get_basis());
    checked_write(m_writer, basis);
}

void BinaryCurveFileWriter::write_curve_count(const ICurveWalker& walker)
{
    const std::uint32_t curve_count = static_cast<std::uint32_t>(walker.get_curve_count());
    checked_write(m_writer, curve_count);
}

void BinaryCurveFileWriter::write_curve(const ICurveWalker& walker, const std::uint32_t curve_id, std::uint32_t& vertex_count)
{
    const std::uint32_t count = static_cast<std::uint32_t>(walker.get_vertex_count(curve_id));
    checked_write(m_writer, count);

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, walker.get_vertex(i + vertex_count));

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, walker.get_vertex_width(i + vertex_count));

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, walker.get_vertex_opacity(i + vertex_count));

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, walker.get_vertex_color(i + vertex_count));

    vertex_count += count;
}

}   // namespace foundation
