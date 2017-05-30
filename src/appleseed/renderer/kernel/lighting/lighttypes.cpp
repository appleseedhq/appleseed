
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
#include "lighttypes.h"

namespace renderer
{

LightSource::LightSource()
{
}

LightSource::~LightSource()
{
}

//
// Non-physical light source class implementation
//

NonPhysicalLightSource::NonPhysicalLightSource(const NonPhysicalLightInfo& light)
: m_light(light)
{
}

foundation::Vector3d NonPhysicalLightSource::get_position()  const
{
    foundation::Vector3d test_vec;
    test_vec[0] = 0.0;
    test_vec[1] = 1.0;
    test_vec[2] = 2.0;
    return test_vec;
}

//
// Emitting triangle light source class implementation
//

EmittingTriangleLightSource::EmittingTriangleLightSource(EmittingTriangle& light)
: m_light(light)
{
}

foundation::Vector3d EmittingTriangleLightSource::get_position() const
{
    foundation::Vector3d test_vec;
    test_vec[0] = 0.0;
    test_vec[1] = 1.0;
    test_vec[2] = 2.0;
    return test_vec;
}

}   // namespace renderer