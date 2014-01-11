
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "basegroup.h"

namespace renderer
{

struct BaseGroup::Impl
{
    ColorContainer              m_colors;
    TextureContainer            m_textures;
    TextureInstanceContainer    m_texture_instances;
    AssemblyContainer           m_assemblies;
    AssemblyInstanceContainer   m_assembly_instances;

    explicit Impl(Entity* parent)
      : m_colors(parent)
      , m_textures(parent)
      , m_texture_instances(parent)
      , m_assemblies(parent)
      , m_assembly_instances(parent)
    {
    }
};

BaseGroup::BaseGroup(Entity* parent)
  : impl(new Impl(parent))
{
}

BaseGroup::~BaseGroup()
{
    delete impl;
}

ColorContainer& BaseGroup::colors() const
{
    return impl->m_colors;
}

TextureContainer& BaseGroup::textures() const
{
    return impl->m_textures;
}

TextureInstanceContainer& BaseGroup::texture_instances() const
{
    return impl->m_texture_instances;
}

AssemblyContainer& BaseGroup::assemblies() const
{
    return impl->m_assemblies;
}

AssemblyInstanceContainer& BaseGroup::assembly_instances() const
{
    return impl->m_assembly_instances;
}

}   // namespace renderer
