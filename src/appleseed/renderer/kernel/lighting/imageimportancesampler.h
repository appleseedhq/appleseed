
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_IMAGEIMPORTANCESAMPLER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_IMAGEIMPORTANCESAMPLER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/scalar.h"

namespace renderer
{

template <typename T>
class ImageImportanceSampler
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    template <typename ImageSampler>
    ImageImportanceSampler(
        const size_t                    width,
        const size_t                    height,
        ImageSampler&                   sampler);

    // Destructor.
    ~ImageImportanceSampler();

    // Resample the image and rebuild the CDFs.
    template <typename ImageSampler>
    void rebuild(ImageSampler& sampler);

    // Sample the image and return the coordinates of the chosen pixel,
    // and the probability with which it was chosen.
    void sample(
        const foundation::Vector<T, 2>& s,
        size_t&                         x,
        size_t&                         y,
        T&                              probability) const;

    // Return the probability density function of a given pixel.
    T get_pdf(
        const size_t                    x,
        const size_t                    y) const;

  private:
    typedef foundation::CDF<size_t, T> CDF;

    const size_t    m_width;
    const size_t    m_height;
    const T         m_rcp_pixel_count;

    CDF*            m_cdf_x;
    CDF             m_cdf_y;
};


//
// ImageImportanceSampler class implementation.
//

template <typename T>
template <typename ImageSampler>
ImageImportanceSampler<T>::ImageImportanceSampler(
    const size_t                    width,
    const size_t                    height,
    ImageSampler&                   sampler)
  : m_width(width)
  , m_height(height)
  , m_rcp_pixel_count(T(1.0) / (width * height))
{
    m_cdf_x = new CDF[m_height];

    rebuild(sampler);
}

template <typename T>
ImageImportanceSampler<T>::~ImageImportanceSampler()
{
    delete [] m_cdf_x;
}

template <typename T>
template <typename ImageSampler>
void ImageImportanceSampler<T>::rebuild(ImageSampler& sampler)
{
    m_cdf_y.clear();

    for (size_t y = 0; y < m_height; ++y)
    {
        m_cdf_x[y].clear();

        for (size_t x = 0; x < m_width; ++x)
        {
            const T importance = sampler(x, y);
            m_cdf_x[y].insert(x, importance);
        }

        if (m_cdf_x[y].valid())
            m_cdf_x[y].prepare();

        m_cdf_y.insert(y, m_cdf_x[y].weight());
    }

    if (m_cdf_y.valid())
        m_cdf_y.prepare();
}

template <typename T>
inline void ImageImportanceSampler<T>::sample(
    const foundation::Vector<T, 2>& s,
    size_t&                         x,
    size_t&                         y,
    T&                              probability) const
{
    if (m_cdf_y.valid())
    {
        const typename CDF::ItemWeightPair ry = m_cdf_y.sample(s[1]);
        const typename CDF::ItemWeightPair rx = m_cdf_x[ry.first].sample(s[0]);

        x = rx.first;
        y = ry.first;

        probability = rx.second * ry.second;
    }
    else
    {
        x = foundation::truncate<size_t>(s[0] * m_width);
        y = foundation::truncate<size_t>(s[1] * m_height);

        probability = m_rcp_pixel_count;
    }
}

template <typename T>
inline T ImageImportanceSampler<T>::get_pdf(
    const size_t                    x,
    const size_t                    y) const
{
    if (m_cdf_y.valid())
    {
        if (m_cdf_x[y].valid())
        {
            const typename CDF::ItemWeightPair ry = m_cdf_y[y];
            const typename CDF::ItemWeightPair rx = m_cdf_x[y][x];

            return rx.second * ry.second;
        }
        else
        {
            return T(0.0);
        }
    }
    else
    {
        return m_rcp_pixel_count;
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_IMAGEIMPORTANCESAMPLER_H
