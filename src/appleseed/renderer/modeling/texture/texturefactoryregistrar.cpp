
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/entity/registerentityfactories.h"
#include "renderer/modeling/texture/disktexture2d.h"
#include "renderer/modeling/texture/memorytexture2d.h"
#include "renderer/modeling/texture/texturetraits.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/registrar.h"

// Standard headers.
#include <cassert>
#include <string>
#include <utility>

using namespace foundation;
using namespace std;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(TextureFactoryArray);

struct TextureFactoryRegistrar::Impl
{
    Registrar<ITextureFactory> m_registrar;
};

TextureFactoryRegistrar::TextureFactoryRegistrar(const SearchPaths& search_paths)
  : impl(new Impl())
{
    reinitialize(search_paths);
}

TextureFactoryRegistrar::~TextureFactoryRegistrar()
{
    delete impl;
}

void TextureFactoryRegistrar::reinitialize(const SearchPaths& search_paths)
{
    // The registrar must be cleared before the plugins are unloaded.
    impl->m_registrar.clear();
    unload_all_plugins();

    // Register built-in factories.
    register_factory(auto_release_ptr<FactoryType>(new DiskTexture2dFactory()));
    register_factory(auto_release_ptr<FactoryType>(new MemoryTexture2dFactory()));
    // Register factories defined in plugins.
   /* register_factories_from_plugins<Texture>(
        search_paths,
        [this](void* plugin_entry_point)
        {
            auto create_fn = reinterpret_cast<ITextureFactory* (*)()>(plugin_entry_point);
            register_factory(foundation::auto_release_ptr<ITextureFactory>(create_fn()));
        });
*/
    collect_plugins<Texture>(
            [this](void* plugin_entry_point)
            {
                auto create_fn = reinterpret_cast<ITextureFactory* (*)()>(plugin_entry_point);
                register_factory(foundation::auto_release_ptr<ITextureFactory>(create_fn()));
            }
    );
}

TextureFactoryArray TextureFactoryRegistrar::get_factories() const
{
    FactoryArrayType factories;

    for (const_each<Registrar<FactoryType>::Items> i = impl->m_registrar.items(); i; ++i)
        factories.push_back(i->second);

    return factories;
}

const TextureFactoryRegistrar::FactoryType* TextureFactoryRegistrar::lookup(const char* name) const
{
    assert(name);
    return impl->m_registrar.lookup(name);
}

void TextureFactoryRegistrar::register_factory(auto_release_ptr<FactoryType> factory)
{
    const string model = factory->get_model();
    impl->m_registrar.insert(model, move(factory));
}

}   // namespace renderer
