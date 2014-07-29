
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingpoint.h"

namespace renderer
{

//
// RendererServices class implementation.
//

RendererServices::RendererServices(
    const Project&          project,
    OIIO::TextureSystem&    texture_sys)
  : OSL::RendererServices()
  , m_project(project)
  , m_texture_sys(texture_sys)
{
}

bool RendererServices::texture(
    OSL::ustring            filename,
    OSL::TextureOpt&        options,
    OSL::ShaderGlobals*     sg,
    float                   s,
    float                   t,
    float                   dsdx,
    float                   dtdx,
    float                   dsdy,
    float                   dtdy,
    float*                  result)
{
    const bool status =
        m_texture_sys.texture(filename, options, s, t, dsdx, dtdx, dsdy, dtdy, result);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::texture3d(
    OSL::ustring            filename,
    OSL::TextureOpt&        options,
    OSL::ShaderGlobals*     sg,
    const OSL::Vec3&        P,
    const OSL::Vec3&        dPdx,
    const OSL::Vec3&        dPdy,
    const OSL::Vec3&        dPdz,
    float*                  result)
{
    const bool status =
        m_texture_sys.texture3d(filename, options, P, dPdx, dPdy, dPdz, result);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::environment(
    OSL::ustring            filename,
    OSL::TextureOpt&        options,
    OSL::ShaderGlobals*     sg,
    const OSL::Vec3&        R,
    const OSL::Vec3&        dRdx,
    const OSL::Vec3&        dRdy,
    float*                  result)
{
    const bool status =
        m_texture_sys.environment(filename, options, R, dRdx, dRdy, result);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::get_texture_info(
    OSL::ustring            filename,
    int                     subimage,
    OSL::ustring            dataname,
    OSL::TypeDesc           datatype,
    void*                   data)
{
    const bool status =
        m_texture_sys.get_texture_info(filename, subimage, dataname, datatype, data);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform,
    float                   time)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    result = obj_xform->get_transform(time);
    return true;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform,
    float                   time)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    result = obj_xform->get_inverse_transform(time);
    return true;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    if (obj_xform->is_animated())
        return false;

    result = obj_xform->get_transform();
    return true;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    if (obj_xform->is_animated())
        return false;

    result = obj_xform->get_inverse_transform();
    return true;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OIIO::ustring           from,
    float                   time)
{
    return false;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OIIO::ustring           from)
{
    return false;
}

bool RendererServices::get_attribute(
    OSL::ShaderGlobals*     sg,
    bool                    derivatives,
    OIIO::ustring           object,
    OIIO::TypeDesc          type,
    OIIO::ustring           name,
    void*                   val)
{
    return false;
}

bool RendererServices::get_array_attribute(
    OSL::ShaderGlobals*     sg,
    bool                    derivatives,
    OIIO::ustring           object,
    OIIO::TypeDesc          type,
    OIIO::ustring           name,
    int                     index,
    void*                   val)
{
    return false;
}

bool RendererServices::get_userdata(
    bool                    derivatives,
    OIIO::ustring           name,
    OIIO::TypeDesc          type,
    OSL::ShaderGlobals*     sg,
    void*                   val)
{
    return false;
}

bool RendererServices::has_userdata(
    OIIO::ustring           name,
    OIIO::TypeDesc          type,
    OSL::ShaderGlobals*     sg)
{
    return false;
}

void RendererServices::log_error(const std::string& message)
{
    if (!message.empty())
        RENDERER_LOG_ERROR("%s", message.c_str());
}

}   // namespace renderer
