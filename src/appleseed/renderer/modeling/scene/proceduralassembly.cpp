
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "proceduralassembly.h"

// Standard headers.
#include <string>

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

namespace renderer
{

//
// ProceduralAssembly class implementation.
//

ProceduralAssembly::ProceduralAssembly(
    const char*         name,
    const ParamArray&   params)
  : Assembly(name, params)
{
}

void ProceduralAssembly::swap_contents(Assembly& assembly)
{
    assemblies().swap(assembly.assemblies());
    assembly_instances().swap(assembly.assembly_instances());
    bsdfs().swap(assembly.bsdfs());
    bssrdfs().swap(assembly.bssrdfs());
    colors().swap(assembly.colors());
    edfs().swap(assembly.edfs());
    lights().swap(assembly.lights());
    materials().swap(assembly.materials());
    objects().swap(assembly.objects());
    object_instances().swap(assembly.object_instances());
    volumes().swap(assembly.volumes());
    shader_groups().swap(assembly.shader_groups());
    surface_shaders().swap(assembly.surface_shaders());
    textures().swap(assembly.textures());
    texture_instances().swap(assembly.texture_instances());
}

}   // namespace renderer
