
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "closures.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// OSL headers.
#include <OSL/genclosure.h>
#include <OSL/oslclosure.h>

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace renderer;

namespace
{
    //
    // Closure Parameters.
    //

    struct EmptyClosureParams {};

    struct AshikhminShirleyClosureParams
    {
        OSL::Vec3 N;
        OSL::Vec3 T;
        OSL::Color3 kd;
        OSL::Color3 ks;
        float nu;
        float nv;
    };

    struct LambertClosureParams
    {
        OSL::Vec3 N;
    };

    struct MicrofacetBRDFClosureParams
    {
        OSL::Vec3 N;
        float glossiness;
    };

    struct RefractionClosureParams
    {
        OSL::Vec3 N;
        float from_ior;
        float to_ior;
    };

    struct ReflectionClosureParams
    {
        OSL::Vec3 N;
    };

    struct TranslucentClosureParams
    {
        OSL::Vec3 N;
    };
}

// We probably want to reuse OSL macros to declare closure params 
// and register the closures. We can use them only inside the OSL namespace.
OSL_NAMESPACE_ENTER

void register_appleseed_closures(OSL::ShadingSystem& shading_system)
{
    // Describe the memory layout of each closure type to the OSL runtime.
    const size_t MaxParams = 32;
    struct BuiltinClosures
    {
        const char*     name;
        int             id;
        ClosureParam    params[MaxParams];
    };

    static const BuiltinClosures builtins[] =
    {
        { "ashikhmin_shirley", AshikhminShirleyID, { CLOSURE_VECTOR_PARAM(AshikhminShirleyClosureParams, N),
                                                     CLOSURE_VECTOR_PARAM(AshikhminShirleyClosureParams, T),
                                                     CLOSURE_COLOR_PARAM(AshikhminShirleyClosureParams, kd),
                                                     CLOSURE_COLOR_PARAM(AshikhminShirleyClosureParams, ks),
                                                     CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, nu),
                                                     CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, nv),
                                                     CLOSURE_FINISH_PARAM(AshikhminShirleyClosureParams) } },

        { "diffuse", LambertID, { CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                  CLOSURE_FINISH_PARAM(LambertClosureParams) } },

        { "emission", EmissionID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "holdout", HoldoutID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },
        
        { "microfacet_beckmann", MicrofacetBeckmannID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                                         CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                                         CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "microfacet_blinn", MicrofacetBlinnID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                                   CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                                   CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "microfacet_ggx", MicrofacetGGXID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                               CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                               CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "microfacet_ward", MicrofacetWardID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                                 CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                                 CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "reflection", ReflectionID, { CLOSURE_VECTOR_PARAM(ReflectionClosureParams, N),
                                        CLOSURE_FINISH_PARAM(ReflectionClosureParams) } },

        { "refraction", RefractionID, { CLOSURE_VECTOR_PARAM(RefractionClosureParams, N),
                                        CLOSURE_FLOAT_PARAM(RefractionClosureParams, from_ior),
                                        CLOSURE_FLOAT_PARAM(RefractionClosureParams, to_ior),
                                        CLOSURE_FINISH_PARAM(RefractionClosureParams) } },

        { "translucent", TranslucentID, { CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                          CLOSURE_FINISH_PARAM(LambertClosureParams) } },

        { "transparency", TransparentID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },
        { 0, 0, {} }    // mark end of the array
    };

    for (size_t i = 0; builtins[i].name != 0; ++i)
    {
        shading_system.register_closure(
            builtins[i].name,
            builtins[i].id,
            builtins[i].params,
            0,
            0);

        RENDERER_LOG_INFO("registered OSL closure %s.", builtins[i].name);
    }
}

OSL_NAMESPACE_EXIT

namespace renderer
{

void register_closures(OSL::ShadingSystem& shading_system)
{
    OSL::register_appleseed_closures(shading_system);
}

}   // namespace renderer
