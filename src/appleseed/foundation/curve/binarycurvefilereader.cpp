
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
#include "binarycurvefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/curve/icurvebuilder.h"
#include "foundation/image/color.h"
#include "foundation/math/vector.h"
#include "foundation/utility/bufferedfile.h"

// Standard headers.
#include <cstdint>
#include <cstring>
#include <memory>

namespace foundation
{

//
// BinaryCurveFileReader class implementation.
//

BinaryCurveFileReader::BinaryCurveFileReader(const std::string& filename)
  : m_filename(filename)
{
}

void BinaryCurveFileReader::read(ICurveBuilder& builder)
{
    BufferedFile file(
        m_filename.c_str(),
        BufferedFile::BinaryType,
        BufferedFile::ReadMode);

    if (!file.is_open())
        throw ExceptionIOError();

    read_and_check_signature(file);

    std::uint16_t version;
    checked_read(file, version);

    std::unique_ptr<ReaderAdapter> reader;

    switch (version)
    {
      // Uncompressed.
      case 1:
        reader.reset(new PassthroughReaderAdapter(file));
        break;

      // LZ4-compressed.
      case 2:
        reader.reset(new LZ4CompressedReaderAdapter(file));
        break;

      // Unknown format.
      default:
        throw ExceptionIOError("unknown binarycurve format version");
    }

    read_curves(*reader.get(), builder);
}

void BinaryCurveFileReader::read_and_check_signature(BufferedFile& file)
{
    static const char ExpectedSig[11] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'C', 'U', 'R', 'V', 'E' };

    char signature[sizeof(ExpectedSig)];
    checked_read(file, signature, sizeof(signature));

    if (memcmp(signature, ExpectedSig, sizeof(ExpectedSig)) != 0)
        throw ExceptionIOError("invalid binarycurve format signature");
}

void BinaryCurveFileReader::read_curves(ReaderAdapter& reader, ICurveBuilder& builder)
{
    try
    {
        while (true)
        {
            // Read the basis and curve count.
            unsigned char curve_basis;
            std::uint32_t curve_count;
            try
            {
                checked_read(reader, curve_basis);
                checked_read(reader, curve_count);
            }
            catch (const ExceptionEOF&)
            {
                // Expected EOF.
                break;
            }

            if (curve_basis < 1 || curve_basis > 4)
                throw ExceptionIOError();

            builder.begin_curve_object(static_cast<CurveBasis>(curve_basis), curve_count);

            for (std::uint32_t i = 0; i < curve_count; ++i)
            {
                builder.begin_curve();
                read_curve(reader, builder);
                builder.end_curve();
            }

            builder.end_curve_object();
        }
    }
    catch (const ExceptionEOF&)
    {
        // Unexpected EOF.
        throw ExceptionIOError();
    }
}

void BinaryCurveFileReader::read_curve(ReaderAdapter &reader, ICurveBuilder &builder)
{
    std::uint32_t vertex_count;
    checked_read(reader, vertex_count);

    for (std::uint32_t i = 0; i < vertex_count; ++i)
    {
        Vector3f v;
        checked_read(reader, v);
        builder.push_vertex(v);
    }

    for (std::uint32_t i = 0; i < vertex_count; ++i)
    {
        float v;
        checked_read(reader, v);
        builder.push_vertex_width(v);
    }

    for (std::uint32_t i = 0; i < vertex_count; ++i)
    {
        float v;
        checked_read(reader, v);
        builder.push_vertex_opacity(v);
    }

    for (std::uint32_t i = 0; i < vertex_count; ++i)
    {
        Color3f v;
        checked_read(reader, v);
        builder.push_vertex_color(v);
    }
}

}   // namespace foundation
