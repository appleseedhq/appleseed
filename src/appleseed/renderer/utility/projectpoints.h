
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer      { class Camera; }

namespace renderer
{

//
// ProjectPoints class.
//

class APPLESEED_DLLSYMBOL ProjectPoints
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    ProjectPoints(
        foundation::auto_release_ptr<Camera> camera,
        const foundation::Vector2u&          resolution);

    // Destructor.
    ~ProjectPoints();

    // Return true if ProjectPoints was correctly initialized.
    bool is_initialized() const;

    // Project a 3D point back to the film plane. The input point is expressed in
    // world space. The returned point is expressed in normalized device coordinates.
    // Returns true if the projection was possible, false otherwise.
    bool project_point(
        const float                          time,
        const foundation::Vector3d&          point,
        foundation::Vector2d&                ndc) const;

    // Similar to project_point(), except that the input point is expressed in camera space.
    bool project_camera_space_point(
        const foundation::Vector3d&          point,
        foundation::Vector2d&                ndc) const;

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace renderer
