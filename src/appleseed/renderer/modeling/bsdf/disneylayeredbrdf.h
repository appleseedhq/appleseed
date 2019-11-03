
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/bsdf.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class DisneyMaterial; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Disney layered BRDF class.
//

class DisneyLayeredBRDF
  : public BSDF
{
  public:
    void release() override;

    const char* get_model() const override;

    bool on_frame_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;

    size_t compute_input_data_size() const override;

    void* evaluate_inputs(
        const ShadingContext&           shading_context,
        const ShadingPoint&             shading_point) const override;

    void sample(
        SamplingContext&                sampling_context,
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const LocalGeometry&            local_geometry,
        const foundation::Dual3f&       outgoing,
        const int                       modes,
        BSDFSample&                     sample) const override;

    float evaluate(
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const LocalGeometry&            local_geometry,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes,
        DirectShadingComponents&        value) const override;

    float evaluate_pdf(
        const void*                     data,
        const bool                      adjoint,
        const LocalGeometry&            local_geometry,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes) const override;

  private:
    friend class DisneyMaterial;

    const DisneyMaterial*               m_parent;
    foundation::auto_release_ptr<BSDF>  m_brdf;

    explicit DisneyLayeredBRDF(const DisneyMaterial* parent);
};

}   // namespace renderer
