
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
#include "mitshairfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/curve/curvebasis.h"
#include "foundation/curve/icurvebuilder.h"
#include "foundation/image/color.h"
#include "foundation/math/fp.h"
#include "foundation/math/vector.h"
#include "foundation/memory/memory.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cstdint>
#include <cstring>
#include <memory>

namespace foundation
{

//
// MitsHairFileReader class implementation.
//

MitsHairFileReader::MitsHairFileReader(const std::string& filename, const float radius, const size_t degree)
  : m_filename(filename)
  , m_radius(radius)
  , m_degree(degree)
{
}

void MitsHairFileReader::read(ICurveBuilder& builder)
{
    BufferedFile file(
        m_filename.c_str(),
        BufferedFile::BinaryType,
        BufferedFile::ReadMode);

    if (!file.is_open())
        throw ExceptionIOError();

    read_and_check_signature(file);

    std::unique_ptr<ReaderAdapter> reader;
    reader.reset(new PassthroughReaderAdapter(file));
    read_curves(*reader.get(), builder);
}

void MitsHairFileReader::read_and_check_signature(BufferedFile& file)
{
    static const char ExpectedSig[11] = { 'B', 'I', 'N', 'A', 'R', 'Y', '_', 'H', 'A', 'I', 'R' };

    char signature[sizeof(ExpectedSig)];
    checked_read(file, signature, sizeof(signature));

    if (memcmp(signature, ExpectedSig, sizeof(ExpectedSig)) != 0)
        throw ExceptionIOError("invalid mitshair format signature");
}

void MitsHairFileReader::read_curves(ReaderAdapter& reader, ICurveBuilder& builder)
{
    try
    {
        // Read the basis and curve count
        std::uint32_t vertex_count;
        try
        {
            checked_read(reader, vertex_count);
        }
        catch (const ExceptionEOF&)
        {
            // Expected EOF
        }

        builder.begin_curve_object(static_cast<CurveBasis>(m_degree));
        builder.begin_curve();

        std::vector<Vector3f> vertices, new_vertices;

        for (std::uint32_t c = 0; c < vertex_count; ++c)
        {
            float x;
            checked_read(reader, x);

            if (FP<float>::is_inf(x))
            {
                switch (m_degree)
                {
                  case 1:
                    if (vertices.size() >= 2)
                    {
                        for (size_t i = 0; i < vertices.size(); ++i)
                            push_vertex_properties(vertices[i], builder);
                        builder.end_curve();
                        builder.begin_curve();
                    }
                    break;

                  case 2:
                  case 3:
                  case 4:
                    if (vertices.size() >= 4)
                    {
                        assert(new_vertices.empty());
                        for (size_t i = 0; i < vertices.size(); ++i)
                        {
                            new_vertices.push_back(vertices[i]);

                            if (i > 0 && i % 2 == 0 && i + 1 < vertices.size())
                            {
                                // Add a midpoint.
                                new_vertices.push_back(0.5f * (vertices[i] + vertices[i + 1]));
                            }
                        }

                        for (size_t i = 0, e = new_vertices.size(); i + 3 < e; i += 3)
                            push_vertex_properties(new_vertices[i], builder);
                        builder.end_curve();
                        builder.begin_curve();
                    }
                    break;

                  assert_otherwise;
                }

                clear_keep_memory(vertices);
                clear_keep_memory(new_vertices);

                continue;
            }

            Vector2f yz;
            checked_read(reader, yz);
            vertices.emplace_back(x, yz[0], yz[1]);
        }

        builder.end_curve_object();
    }
    catch (const ExceptionEOF&)
    {
        // Unexpected EOF.
        throw ExceptionIOError();
    }
}

void MitsHairFileReader::push_vertex_properties(const Vector3f& v, ICurveBuilder& builder)
{
    builder.push_vertex(v);
    builder.push_vertex_width(m_radius);
    builder.push_vertex_opacity(1.0f);                      // default opacity
    builder.push_vertex_color(Color3f(0.2f, 0.0f, 0.7f));   // default color
}

}   // namespace foundation
