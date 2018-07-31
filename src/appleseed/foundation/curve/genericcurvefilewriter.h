
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

#ifndef APPLESEED_FOUNDATION_CURVE_GENERICCURVEFILEWRITER_H
#define APPLESEED_FOUNDATION_CURVE_GENERICCURVEFILEWRITER_H

// appleseed.foundation headers.
#include "foundation/curve/icurvefilewriter.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class ICurveWalker; }

namespace foundation
{

//
// Write a curve file using the right writer based on the extension of the curve file name.
//

class APPLESEED_DLLSYMBOL GenericCurveFileWriter
  : public ICurveFileWriter
{
  public:
    // Constructor.
    explicit GenericCurveFileWriter(const char* filename);

    // Destructor.
    ~GenericCurveFileWriter() override;

    // Write a curve object.
    void write(const ICurveWalker& walker) override;

  private:
    ICurveFileWriter* m_writer;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_CURVE_GENERICCURVEFILEWRITER_H
