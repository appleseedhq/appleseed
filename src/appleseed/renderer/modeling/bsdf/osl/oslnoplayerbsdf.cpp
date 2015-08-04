
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 [Author here...]
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

// Interface header.
#include "oslnoplayerbsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/closures.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    struct APPLESEED_ALIGN(16) OSLLayerHelper
    {
        OSLLayerHelper(
            const BSDF*         osl_bsdf,
            const Basis3d&      shading_basis,
            void*               ci)
        {
            OSL::ClosureColor* closure_tree = reinterpret_cast<OSL::ClosureColor*>(ci);
            CompositeSurfaceClosure* c = reinterpret_cast<CompositeSurfaceClosure*>(data);
            new (c) CompositeSurfaceClosure(osl_bsdf, shading_basis, closure_tree);
        }

        // Must be first (alignment).
        uint8 data[InputEvaluator::DataSize];
    };

    //
    // OSL Nop Layer BSDF.
    //

    const char* Model = "osl_nop_layer_btdf";

    class OSLNopLayerBSDFImpl
      : public BSDF
    {
      public:
        OSLNopLayerBSDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, AllBSDFTypes, BSDFSample::AllScatteringModes, params)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        FORCE_INLINE virtual void sample(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const
        {
            const OSLNopLayerBSDFInputValues* values =
                reinterpret_cast<const OSLNopLayerBSDFInputValues*>(data);

            OSLLayerHelper helper(values->m_osl_bsdf, sample.get_shading_basis(), values->m_base);
            values->m_osl_bsdf->sample(
                helper.data,
                adjoint,
                cosine_mult,
                sample);
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const
        {
            const OSLNopLayerBSDFInputValues* values =
                reinterpret_cast<const OSLNopLayerBSDFInputValues*>(data);

            OSLLayerHelper helper(values->m_osl_bsdf, shading_basis, values->m_base);
            const double pdf = values->m_osl_bsdf->evaluate(
                helper.data,
                adjoint,
                cosine_mult,
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                modes,
                value);

            return pdf;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            const OSLNopLayerBSDFInputValues* values =
                reinterpret_cast<const OSLNopLayerBSDFInputValues*>(data);

            OSLLayerHelper helper(values->m_osl_bsdf, shading_basis, values->m_base);
            const double pdf = values->m_osl_bsdf->evaluate_pdf(
                helper.data,
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                modes);

            return pdf;
        }
    };

    typedef BSDFWrapper<OSLNopLayerBSDFImpl> OSLNopLayerBSDF;
}


//
// OSLNopLayerBSDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLNopLayerBSDFFactory::create() const
{
    return auto_release_ptr<BSDF>(new OSLNopLayerBSDF("osl_nop_layer", ParamArray()));
}

}   // namespace renderer
