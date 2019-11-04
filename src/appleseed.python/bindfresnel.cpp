
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    bpy::tuple af_conductor_reparam(
        const double r,
        const double g)
    {
        double n, k;
        artist_friendly_fresnel_conductor_reparameterization(
            r,
            g,
            n,
            k);

        return bpy::make_tuple(n, k);
    }

    bpy::tuple af_conductor_inverse_reparam(
        const double n,
        const double k)
    {
        double r, g;
        artist_friendly_fresnel_conductor_inverse_reparameterization(
            n,
            k,
            r,
            g);

        return bpy::make_tuple(r, g);
    }
}

void bind_fresnel()
{
    bpy::def(
        "artist_friendly_fresnel_conductor_reparameterization",
        &af_conductor_reparam);

    bpy::def(
        "artist_friendly_fresnel_conductor_inverse_reparameterization",
        &af_conductor_inverse_reparam);
}
