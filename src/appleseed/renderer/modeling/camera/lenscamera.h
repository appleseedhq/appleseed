
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2022 Lars Zawallich, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/camera/perspectivecamera.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"

// appleseed.foundation headers.

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }

using namespace foundation;

namespace renderer
{

//
// Lens camera base class.
//

class LensCamera : public PerspectiveCamera
{
  public:
    LensCamera(
      const char*                     name,
      const ParamArray&               params);

  protected:
    class ImageSampler
    {
      public:
        ImageSampler(
            TextureCache&   texture_cache,
            const Source*   source,
            const size_t    width,
            const size_t    height)
          : m_texture_cache(texture_cache)
          , m_source(source)
          , m_width(width)
          , m_height(height)
          , m_range(std::sqrt(1.0 + static_cast<double>(m_height * m_height) / (m_width * m_width)))
        {
        }

        void sample(const size_t x, const size_t y, Vector2d& payload, float& importance) const
        {
            payload = Vector2d(
                (2.0 * x + 1.0 - m_width) / (m_width - 1.0),
                (2.0 * y + 1.0 - m_height) / (m_height - 1.0));

            if (m_height != m_width)
                payload.y *= static_cast<double>(m_height) / m_width;

            payload /= m_range;     // scale to fit in a unit disk

            const Vector2f uv(
                x / (m_width - 1.0f),
                y / (m_height - 1.0f));

            Color3f color;
            m_source->evaluate(m_texture_cache, SourceInputs(uv), color);

            importance = luminance(color);
        }

      private:
        TextureCache&       m_texture_cache;
        const Source*       m_source;
        const size_t        m_width;
        const size_t        m_height;
        const double        m_range;
    };

    void extract_diaphragm_blade_count();
    void extract_diaphragm_tilt_angle();
    void extract_focal_distance(const bool autofocus_enabled, Vector2d& autofocus_target, double& focal_distance) const;

    size_t                   m_diaphragm_blade_count;    // number of blades of the diaphragm, 0 for round aperture
    double                   m_diaphragm_tilt_angle;     // tilt angle of the diaphragm in radians
};

//
// An incomplete factory class whose main purpose is to factorize the code
// common to all lens camera models.
//

class APPLESEED_DLLSYMBOL LensCameraFactory
{
  public:
    static void add_lens_metadata(foundation::DictionaryArray& metadata);
};

}   // namespace renderer
