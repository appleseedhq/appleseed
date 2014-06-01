
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
#include "disneymaterial.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/disneylayeredbdrf.h"

using namespace foundation;

namespace renderer
{

//
// DisneyMaterialLayer class implementation.
//

struct DisneyMaterialLayer::Impl
{
};

DisneyMaterialLayer::DisneyMaterialLayer()
  : impl(new Impl())
{
}

DisneyMaterialLayer::~DisneyMaterialLayer()
{
    delete impl;
}

//
// DisneyMaterial class implementation.
//

namespace
{

const char* Model = "disney_material";

}

struct DisneyMaterial::Impl
{
    foundation::auto_release_ptr<DisneyLayeredBRDF> m_brdf;
};

DisneyMaterial::DisneyMaterial(
    const char*         name,
    const ParamArray&   params)
    : Material(name, params)
    , impl(new Impl())
{
}

DisneyMaterial::~DisneyMaterial()
{
    delete impl;
}

void DisneyMaterial::release()
{
    delete this;
}

const char* DisneyMaterial::get_model() const
{
    return Model;
}

bool DisneyMaterial::on_frame_begin(
    const Project&              project,
    const Assembly&             assembly,
    AbortSwitch*                abort_switch)
{
    if (!Material::on_frame_begin(project, assembly, abort_switch))
        return false;

    return true;
}

// This method is called once after rendering each frame.
void DisneyMaterial::on_frame_end(
    const Project&              project,
    const Assembly&             assembly) OVERRIDE
{
    Material::on_frame_end(project, assembly);
}

//
// DisneyMaterialFactory class implementation.
//

DictionaryArray DisneyMaterialFactory::get_input_metadata()
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<DisneyMaterial> DisneyMaterialFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<DisneyMaterial>(
            new DisneyMaterial(name, params));
}

}   // namespace renderer
