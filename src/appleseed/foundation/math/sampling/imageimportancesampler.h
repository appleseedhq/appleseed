
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_SAMPLING_IMAGEIMPORTANCESAMPLER_H
#define APPLESEED_FOUNDATION_MATH_SAMPLING_IMAGEIMPORTANCESAMPLER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/cdf.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/job/iabortswitch.h"

// Standard headers.
#include <cstddef>
#include <utility>

namespace foundation
{

//
// The ImageSampler type must conform to the following prototype:
//
//   class ImageSampler
//   {
//     public:
//       void sample(
//           const size_t   x,
//           const size_t   y,
//           Payload&       payload,
//           Importance&    importance);
//   };
//

template <typename Payload, typename Importance>
class ImageImportanceSampler
  : public NonCopyable
{
  public:
    typedef Vector<Importance, 2> Vector2Type;

    // Constructor.
    ImageImportanceSampler(
        const size_t        width,
        const size_t        height);

    // Destructor.
    ~ImageImportanceSampler();

    // Resample the image and rebuild the CDFs.
    template <typename ImageSampler>
    void rebuild(
        ImageSampler&       sampler,
        IAbortSwitch*       abort_switch = 0);

    // Sample the image and return the coordinates of the chosen pixel
    // as well as its probability density.
    void sample(
        const Vector2Type&  s,
        Payload&            payload,
        size_t&             y,
        Importance&         probability) const;

    // Return the probability density of a given pixel.
    Importance get_pdf(
        const size_t        x,
        const size_t        y) const;

  private:
    typedef CDF<size_t, Importance> YCDF;
    typedef CDF<Payload, Importance> XCDF;

    const size_t            m_width;
    const size_t            m_height;
    const Importance        m_rcp_pixel_count;

    XCDF*                   m_cdf_x;
    YCDF                    m_cdf_y;
};


//
// ImageImportanceSampler class implementation.
//

template <typename Payload, typename Importance>
ImageImportanceSampler<Payload, Importance>::ImageImportanceSampler(
    const size_t            width,
    const size_t            height)
  : m_width(width)
  , m_height(height)
  , m_rcp_pixel_count(Importance(1.0) / (width * height))
{
    m_cdf_x = new XCDF[m_height];
}

template <typename Payload, typename Importance>
ImageImportanceSampler<Payload, Importance>::~ImageImportanceSampler()
{
    delete [] m_cdf_x;
}

template <typename Payload, typename Importance>
template <typename ImageSampler>
void ImageImportanceSampler<Payload, Importance>::rebuild(
    ImageSampler&           sampler,
    IAbortSwitch*           abort_switch)
{
    m_cdf_y.clear();
    m_cdf_y.reserve(m_height);

    for (size_t y = 0; y < m_height; ++y)
    {
        if (is_aborted(abort_switch))
        {
            m_cdf_y.clear();
            break;
        }

        m_cdf_x[y].clear();
        m_cdf_x[y].reserve(m_width);

        for (size_t x = 0; x < m_width; ++x)
        {
            Payload payload;
            Importance importance;

            sampler.sample(x, y, payload, importance);

            m_cdf_x[y].insert(payload, importance);
        }

        if (m_cdf_x[y].valid())
            m_cdf_x[y].prepare();

        m_cdf_y.insert(y, m_cdf_x[y].weight());
    }

    if (m_cdf_y.valid())
        m_cdf_y.prepare();
}

template <typename Payload, typename Importance>
inline void ImageImportanceSampler<Payload, Importance>::sample(
    const Vector2Type&      s,
    Payload&                payload,
    size_t&                 y,
    Importance&             probability) const
{
    if (m_cdf_y.valid())
    {
        const typename YCDF::ItemWeightPair ry = m_cdf_y.sample(s[1]);
        const typename XCDF::ItemWeightPair rx = m_cdf_x[ry.first].sample(s[0]);

        payload = rx.first;
        y = ry.first;

        probability = rx.second * ry.second;
    }
    else
    {
        const size_t x = truncate<size_t>(s[0] * m_width);

        y = truncate<size_t>(s[1] * m_height);
        payload = m_cdf_x[y][x].first;

        probability = m_rcp_pixel_count;
    }
}

template <typename Payload, typename Importance>
inline Importance ImageImportanceSampler<Payload, Importance>::get_pdf(
    const size_t            x,
    const size_t            y) const
{
    if (m_cdf_y.valid())
    {
        if (m_cdf_x[y].valid())
        {
            const typename YCDF::ItemWeightPair ry = m_cdf_y[y];
            const typename XCDF::ItemWeightPair rx = m_cdf_x[y][x];

            return rx.second * ry.second;
        }
        else
        {
            return Importance(0.0);
        }
    }
    else
    {
        return m_rcp_pixel_count;
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_SAMPLING_IMAGEIMPORTANCESAMPLER_H
