
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_ANIMATECAMERA_ANIMATIONPATH_H
#define APPLESEED_ANIMATECAMERA_ANIMATIONPATH_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/transform.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace foundation    { class Logger; }

namespace appleseed {
namespace animatecamera {

//
// An animation path is a series of rigid transformation keyframes.
//

class AnimationPath
  : public foundation::NonCopyable
{
  public:
    enum Format
    {
        Default,            // appleseed's coordinate system (right-handed, up is Y)
        Autodesk3dsMax      // Autodesk 3ds Max's coordinate system (right-handed, up is Z)
    };

    // Constructor.
    explicit AnimationPath(foundation::Logger& logger);

    // Load an animation path from disk. Returns true on success.
    bool load(const char* filename, const Format format = Default);

    // Return the number of keyframes in the animation path.
    size_t size() const;

    // Return the i'th keyframe of the animation path.
    const foundation::Transformd& operator[](const size_t i) const;

  private:
    foundation::Logger&                 m_logger;
    std::vector<foundation::Transformd> m_keyframes;
};

}       // namespace animatecamera
}       // namespace appleseed

#endif  // !APPLESEED_ANIMATECAMERA_ANIMATIONPATH_H
