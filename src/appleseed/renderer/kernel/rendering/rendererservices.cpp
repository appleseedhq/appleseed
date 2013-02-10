
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2013 Esteban Tovagliari
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
#include "rendererservices.h"

namespace renderer
{

//
// RenderServices class implementation.
//

RendererServices::RendererServices()
  : OSL::RendererServices()
{
}

RendererServices::~RendererServices()
{
}

bool RendererServices::get_matrix(OSL::Matrix44 &result,
                                  OSL::TransformationPtr xform,
                                  float time)
{
    return false;
}

bool RendererServices::get_matrix(OSL::Matrix44 &result,
                                  OSL::TransformationPtr xform)
{
    return false;
}

bool RendererServices::get_matrix(OSL::Matrix44 &result,
                                  OIIO::ustring from,
                                  float time)
{
    return false;
}

bool RendererServices::get_matrix(OSL::Matrix44 &result,
                                  OIIO::ustring from)
{
    return false;
}

bool RendererServices::get_attribute(void *renderstate,
                                     bool derivatives,
                                     OIIO::ustring object,
                                     OIIO::TypeDesc type,
                                     OIIO::ustring name,
                                     void *val)
{
    return false;
}

bool RendererServices::get_array_attribute(void *renderstate,
                                           bool derivatives,
                                           OIIO::ustring object,
                                           OIIO::TypeDesc type,
                                           OIIO::ustring name,
                                           int index,
                                           void *val)
{
    return false;
}

bool RendererServices::get_userdata(bool derivatives,
                                    OIIO::ustring name,
                                    OIIO::TypeDesc type,
                                    void *renderstate,
                                    void *val)
{
    return false;
}

bool RendererServices::has_userdata(OIIO::ustring name,
                                    OIIO::TypeDesc type,
                                    void *renderstate)
{
    return false;
}

}   // namespace renderer
