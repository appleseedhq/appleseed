
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
#include "light.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Light class implementation.
//

struct Light::Impl
{
    // Order of data members impacts performance, preserve it.
    Transformd              m_transform;
    const EDF*              m_edf;
};

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Light::Light(
    const char*             name,
    const Transformd&       transform,
    const EDF*              edf)
  : Entity(g_class_uid)
  , impl(new Impl())
{
    assert(edf);

    set_name(name);

    impl->m_transform = transform;
    impl->m_edf = edf;
}

Light::Light(
    const char*             name,
    const ParamArray&       params,
    const Transformd&       transform,
    const EDFContainer&     edfs)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    impl->m_transform = transform;
    impl->m_edf = get_required_entity<EDF>(edfs, params, "edf");
}

Light::~Light()
{
    delete impl;
}

void Light::release()
{
    delete this;
}

const char* Light::get_model() const
{
    return LightFactory::get_model();
}

const Transformd& Light::get_transform() const
{
    return impl->m_transform;
}

const EDF* Light::get_edf() const
{
    return impl->m_edf;
}


//
// LightFactory class implementation.
//

const char* LightFactory::get_model()
{
    return "generic_light";
}

auto_release_ptr<Light> LightFactory::create(
    const char*             name,
    const Transformd&       transform,
    const EDF*              edf)
{
    return
        auto_release_ptr<Light>(
            new Light(
                name,
                transform,
                edf));
}

auto_release_ptr<Light> LightFactory::create(
    const char*             name,
    const ParamArray&       params,
    const Transformd&       transform,
    const EDFContainer&     edfs)
{
    return
        auto_release_ptr<Light>(
            new Light(
                name,
                params,
                transform,
                edfs));
}

}   // namespace renderer
