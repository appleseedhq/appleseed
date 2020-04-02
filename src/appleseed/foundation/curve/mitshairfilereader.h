
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

#ifndef APPLESEED_FOUNDATION_CURVE_MITSHAIRCURVEFILEREADER_H
#define APPLESEED_FOUNDATION_CURVE_MITSHAIRCURVEFILEREADER_H

// appleseed.foundation headers.
#include "foundation/curve/icurvefilereader.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Forward declarations.
namespace foundation    { class BufferedFile; }
namespace foundation    { class ICurveBuilder; }
namespace foundation    { class ReaderAdapter; }

namespace foundation
{

//
// Read for mitshair curve file format.
//

class MitsHairFileReader
  : public ICurveFileReader
{
  public:
    // Constructor.
    MitsHairFileReader(const std::string& filename, const float radius, const size_t degree);

    // Read a curve file.
    void read(ICurveBuilder& builder) override;

  private:
    const std::string       m_filename;
    const float             m_radius;
    const size_t            m_degree;

    static void read_and_check_signature(BufferedFile& file);

    void read_curves(ReaderAdapter& reader, ICurveBuilder& builder);
    void push_vertex_properties(const Vector3f& v, ICurveBuilder& builder);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_CURVE_MITSHAIRCURVEFILEREADER_H
