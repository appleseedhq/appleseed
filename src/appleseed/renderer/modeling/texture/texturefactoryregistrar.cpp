
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "texturefactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/texture/disktexture2d.h"
#include "renderer/modeling/texture/writabletexture2d.h"

// appleseed.foundation headers.
#include "foundation/utility/registrar.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TextureFactoryRegistrar class implementation.
//

struct TextureFactoryRegistrar::Impl
{
    Registrar<ITextureFactory> m_registrar;
};

TextureFactoryRegistrar::TextureFactoryRegistrar()
  : impl(new Impl())
{
    impl->m_registrar.insert(
        DiskTexture2dFactory::get_model(),
        auto_ptr<ITextureFactory>(new DiskTexture2dFactory()));

    impl->m_registrar.insert(
        WritableTexture2dFactory::get_model(),
        auto_ptr<ITextureFactory>(new WritableTexture2dFactory()));
}

TextureFactoryRegistrar::~TextureFactoryRegistrar()
{
    delete impl;
}

const TextureFactoryRegistrar::FactoryType* TextureFactoryRegistrar::lookup(const char* name) const
{
    assert(name);

    return impl->m_registrar.lookup(name);
}

}   // namespace renderer
