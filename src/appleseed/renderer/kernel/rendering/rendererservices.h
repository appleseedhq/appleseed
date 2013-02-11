
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERER_SERVICES_H
#define APPLESEED_RENDERER_KERNEL_RENDERER_SERVICES_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compiler.h"

// OSL headers
#include "OSL/oslexec.h"

namespace renderer
{

//
// Appleseed's OSL RendererServices.
//

class RendererServices
  : public OSL::RendererServices
{
  public:
    // Constructor
    RendererServices();

    // Destructor
    ~RendererServices();

    // Get the 4x4 matrix that transforms by the specified
    // transformation at the given time.  Return true if ok, false
    // on error.
    virtual bool get_matrix(OSL::Matrix44 &result,
                            OSL::TransformationPtr xform,
                            float time) OVERRIDE;

    // Get the 4x4 matrix that transforms by the specified
    // transformation.  Return true if ok, false on error.  Since no
    // time value is given, also return false if the transformation may
    // be time-varying.
    virtual bool get_matrix(OSL::Matrix44 &result,
                            OSL::TransformationPtr xform) OVERRIDE;

    // Get the 4x4 matrix that transforms points from the named
    // 'from' coordinate system to "common" space at the given time.
    // Returns true if ok, false if the named matrix is not known.
    virtual bool get_matrix(OSL::Matrix44 &result,
                            OIIO::ustring from,
                            float time) OVERRIDE;

    // Get the 4x4 matrix that transforms 'from' to "common" space.
    // Since there is no time value passed, return false if the
    // transformation may be time-varying(as well as if it's not found
    // at all).
    virtual bool get_matrix(OSL::Matrix44 &result,
                            OIIO::ustring from) OVERRIDE;

    // Get the named attribute from the renderer and if found then
    // write it into 'val'.  Otherwise, return false.  If no object is
    // specified (object == ustring()), then the renderer should search *first*
    // for the attribute on the currently shaded object, and next, if
    // unsuccessful, on the currently shaded "scene".
    //
    // Note to renderers: if renderstate is NULL, that means
    // get_attribute is being called speculatively by the runtime
    // optimizer, and it doesn't know which object the shader will be
    // run on. Be robust to this situation, return 'true' (retrieve the
    // attribute) if you can (known object and attribute name), but
    // otherwise just fail by returning 'false'.
    virtual bool get_attribute(void *renderstate,
                               bool derivatives,
                               OIIO::ustring object,
                               OIIO::TypeDesc type,
                               OIIO::ustring name,
                               void *val) OVERRIDE;

    // Similar to get_attribute();  this method will return the 'index'
    // element of an attribute array.
    virtual bool get_array_attribute(void *renderstate,
                                     bool derivatives,
                                     OIIO::ustring object,
                                     OIIO::TypeDesc type,
                                     OIIO::ustring name,
                                     int index,
                                     void *val) OVERRIDE;

    // Get the named user-data from the current object and write it into
    // 'val'. If derivatives is true, the derivatives should be written into val
    // as well. Return false if no user-data with the given name and type was found.
    virtual bool get_userdata(bool derivatives,
                              OIIO::ustring name,
                              OIIO::TypeDesc type,
                              void *renderstate,
                              void *val) OVERRIDE;

    // Does the current object have the named user-data associated with it?
    virtual bool has_userdata(OIIO::ustring name,
                              OIIO::TypeDesc type,
                              void *renderstate) OVERRIDE;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERER_SERVICES_H
