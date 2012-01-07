
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_AOV_AOVCOLLECTION_H
#define APPLESEED_RENDERER_MODELING_AOV_AOVCOLLECTION_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/uid.h"

namespace renderer
{

class AOVCollection
  : public foundation::NonCopyable
{
  public:
    AOVCollection();

    void copy_declarations_from(const AOVCollection& source);

    void declare(const foundation::UniqueID uid);

    void set(const float val);

    Spectrum& operator[](const foundation::UniqueID uid);
    const Spectrum& operator[](const foundation::UniqueID uid) const;

    AOVCollection& operator+=(const AOVCollection& rhs);
    AOVCollection& operator*=(const Spectrum& rhs);
    AOVCollection& operator*=(const float rhs);
    AOVCollection& operator/=(const float rhs);

  private:
    friend class AOVFrameCollection;
    friend class ShadingResult;

    struct AOV
    {
        foundation::UniqueID    m_uid;
        Spectrum                m_spectrum;
    };

    enum { MaxAOVCount = 8 };

    AOV         m_aovs[MaxAOVCount];
    size_t      m_aov_count;

    Spectrum    m_trash;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_AOV_AOVCOLLECTION_H
