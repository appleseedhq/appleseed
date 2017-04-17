
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H
#define APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/modeling/aov/aovcontainer.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace renderer { class Camera; }
namespace renderer { class ShadingPoint; }
namespace renderer { class ShadingResult; }

namespace renderer
{

//
// AOV accumulator base class.
//

class AOVAccumulator
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~AOVAccumulator();

    // Delete this instance.
    virtual void release();

    // Reset the accumulator.
    virtual void reset() = 0;

    // Write a value to the accumulator.
    // Normally, this is used for geometry AOVs like normals, velocity...
    virtual void write(
        const ShadingPoint& shading_point,
        const Camera&       camera);

    // Flush the result.
    virtual void flush(ShadingResult& result) = 0;

  protected:
    // Constructor.
    explicit AOVAccumulator(const size_t index);

    const size_t m_index;
};


//
// BeautyAOVAccumulator class.
//

class BeautyAOVAccumulator
  : public AOVAccumulator
{
  public:
    BeautyAOVAccumulator();

    void set(const Spectrum& value);
    void set(const foundation::Color3f& color);

    void set_to_pink_linear_rgb();

    void apply_multiplier(const float multiplier);

    virtual void reset() APPLESEED_OVERRIDE;

    virtual void flush(ShadingResult& result) APPLESEED_OVERRIDE;

  private:
    foundation::ColorSpace  m_color_space;
    Spectrum                m_color;
};


//
// AlphaAOVAccumulator class.
//

class AlphaAOVAccumulator
  : public AOVAccumulator
{
  public:
    AlphaAOVAccumulator();

    const Alpha& get() const;

    void set(const Alpha& alpha);

    void apply_multiplier(const Alpha& multiplier);

    virtual void reset() APPLESEED_OVERRIDE;

    virtual void flush(ShadingResult& result) APPLESEED_OVERRIDE;

  private:
    Alpha m_alpha;
};


//
// A collection of AOV accumulators.
//

class AOVAccumulatorContainer
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit AOVAccumulatorContainer(const AOVContainer& aovs);

    // Destructor.
    ~AOVAccumulatorContainer();

    // Reset all accumulators.
    void reset();

    // Write a sample to all accumulators.
    void write(
        const ShadingPoint& shading_point,
        const Camera&       camera);

    // Flush all the accumulators.
    void flush(ShadingResult& result);

    // Access the beauty AOV.
    BeautyAOVAccumulator& beauty();

    // Access the alpha AOV.
    AlphaAOVAccumulator& alpha();

  private:
    bool insert(foundation::auto_release_ptr<AOVAccumulator> aov_accum);

    enum { MaxAovAccumulators = MaxAOVCount + 2 };  // MaxAOVCount + Beauty + Alpha

    size_t          m_size;
    AOVAccumulator* m_accumulators[MaxAovAccumulators];
};


//
// AlphaAOVAccumulator class implementation.
//

inline const Alpha& AlphaAOVAccumulator::get() const
{
    return m_alpha;
}


//
// AOVAccumulatorContainer class implementation.
//

inline BeautyAOVAccumulator& AOVAccumulatorContainer::beauty()
{
    return static_cast<BeautyAOVAccumulator&>(*m_accumulators[0]);
}

inline AlphaAOVAccumulator& AOVAccumulatorContainer::alpha()
{
    return static_cast<AlphaAOVAccumulator&>(*m_accumulators[1]);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H
