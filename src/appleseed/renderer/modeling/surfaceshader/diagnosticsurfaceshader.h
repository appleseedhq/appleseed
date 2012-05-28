
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_MODELING_SURFACESHADER_DIAGNOSTICSURFACESHADER_H
#define APPLESEED_RENDERER_MODELING_SURFACESHADER_DIAGNOSTICSURFACESHADER_H

// appleseed.renderer headers.
#include "renderer/modeling/surfaceshader/isurfaceshaderfactory.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/kvpair.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }
namespace renderer      { class ShadingResult; }

namespace renderer
{

//
// A shader offering a variety of diagnostic modes.
//

class RENDERERDLL DiagnosticSurfaceShader
  : public SurfaceShader
{
  public:
    // Available shading modes.
    enum ShadingMode
    {
        Coverage = 0,               // shade according to pixel coverage
        Barycentric,                // shade according to barycentric coordinates
        UV,                         // shade according to UV coordinates
        Tangent,                    // shade according to the tangent vector
        Bitangent,                  // shade according to the bitangent vector
        GeometricNormal,            // shade according to the geometric normal
        ShadingNormal,              // shade according to the (possibly modified) shading normal
        OriginalShadingNormal,      // shade according to the original shading normal
        Sides,                      // shade according to the surface side
        Depth,                      // shade according to distance from camera
        Wireframe,                  // wireframe
        AmbientOcclusion,           // ambient occlusion
        AssemblyInstances,          // assign a unique color to each assembly instance
        ObjectInstances,            // assign a unique color to each object instance
        Regions,                    // assign a unique color to each region
        Triangles,                  // assign a unique color to each triangle
        Materials,                  // assign a unique color to each material
        ShadingModeCount            // number of shading modes -- keep last
    };

    static const foundation::KeyValuePair<const char*, ShadingMode> ShadingModeValues[];
    static const foundation::KeyValuePair<const char*, const char*> ShadingModeNames[];

    // Constructor.
    DiagnosticSurfaceShader(
        const char*             name,
        const ParamArray&       params);

    // Delete this instance.
    virtual void release() override;

    // Return a string identifying the model of this surface shader.
    virtual const char* get_model() const override;

    // Evaluate the shading at a given point.
    virtual void evaluate(
        SamplingContext&        sampling_context,
        const ShadingContext&   shading_context,
        const ShadingPoint&     shading_point,
        ShadingResult&          shading_result) const override;

  private:
    struct Impl;
    Impl* impl;

    // Destructor.
    ~DiagnosticSurfaceShader();

    void extract_parameters();
};


//
// Diagnostic surface shader factory.
//

class RENDERERDLL DiagnosticSurfaceShaderFactory
  : public ISurfaceShaderFactory
{
  public:
    // Return a string identifying this surface shader model.
    virtual const char* get_model() const override;

    // Return a human-readable string identifying this surface shader model.
    virtual const char* get_human_readable_model() const override;

    // Return a set of widget definitions for this surface shader model.
    virtual foundation::DictionaryArray get_widget_definitions() const override;

    // Create a new surface shader instance.
    virtual foundation::auto_release_ptr<SurfaceShader> create(
        const char*         name,
        const ParamArray&   params) const override;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SURFACESHADER_DIAGNOSTICSURFACESHADER_H
