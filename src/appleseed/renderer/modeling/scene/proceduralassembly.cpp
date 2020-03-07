
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"

using namespace foundation;

namespace renderer
{

//
// ProceduralAssembly class implementation.
//

ProceduralAssembly::ProceduralAssembly(
    const char*         name,
    const ParamArray&   params)
  : Assembly(name, params)
  , m_expanded(false)
{
}

bool ProceduralAssembly::expand_contents(
    const Project&      project,
    const Assembly*     parent,
    IAbortSwitch*       abort_switch)
{
    if (m_expanded)
        return true;

    RENDERER_LOG_INFO("expanding procedural assembly \"%s\"...", get_path().c_str());

    if (!do_expand_contents(project, parent, abort_switch))
        return false;

    RENDERER_LOG_INFO(
        "procedural assembly \"%s\" expanded to the following entities:\n"
        "  assemblies                    %s\n"
        "  assembly instances            %s\n"
        "  bsdfs                         %s\n"
        "  bssrdfs                       %s\n"
        "  colors                        %s\n"
        "  edfs                          %s\n"
        "  lights                        %s\n"
        "  materials                     %s\n"
        "  objects                       %s\n"
        "  object instances              %s\n"
        "  shader groups                 %s\n"
        "  surface shaders               %s\n"
        "  textures                      %s\n"
        "  texture instances             %s\n"
        "  volumes                       %s",
        get_path().c_str(),
        pretty_uint(assemblies().size()).c_str(),
        pretty_uint(assembly_instances().size()).c_str(),
        pretty_uint(bsdfs().size()).c_str(),
        pretty_uint(bssrdfs().size()).c_str(),
        pretty_uint(colors().size()).c_str(),
        pretty_uint(edfs().size()).c_str(),
        pretty_uint(lights().size()).c_str(),
        pretty_uint(materials().size()).c_str(),
        pretty_uint(objects().size()).c_str(),
        pretty_uint(object_instances().size()).c_str(),
        pretty_uint(shader_groups().size()).c_str(),
        pretty_uint(surface_shaders().size()).c_str(),
        pretty_uint(textures().size()).c_str(),
        pretty_uint(texture_instances().size()).c_str(),
        pretty_uint(volumes().size()).c_str());

    m_expanded = true;

    return true;
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
    shader_groups().swap(assembly.shader_groups());
    surface_shaders().swap(assembly.surface_shaders());
    textures().swap(assembly.textures());
    texture_instances().swap(assembly.texture_instances());
    volumes().swap(assembly.volumes());
}

}   // namespace renderer
