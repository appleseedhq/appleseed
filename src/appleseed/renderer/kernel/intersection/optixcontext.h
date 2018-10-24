
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
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

#ifndef APPLESEED_RENDERER_KERNEL_GPU_OPTIXCONTEXT_H
#define APPLESEED_RENDERER_KERNEL_GPU_OPTIXCONTEXT_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// OptiX headers.
#include "optixu/optixpp.h"

namespace renderer
{

//
// OptiX context wrapper.
// todo: do we need this class?
//

class OptixContext
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit OptixContext(const int device_number);

    // Destructor.
    ~OptixContext();

    int get_device_number() const;

    // Return the OptiX group representing the scene.
    optix::Group get_scene();

    // Factory methods.

    optix::Transform create_transform();

    optix::GeometryGroup create_geometry_group();

    optix::Geometry create_geometry();
    optix::GeometryInstance create_geometry_instance();

    optix::Acceleration create_acceleration(const char* builder);

    optix::Material create_material();

    optix::Buffer create_buffer(unsigned int type, RTformat format, RTsize width);
    optix::Buffer create_buffer(unsigned int type, RTformat format, RTsize width, RTsize height);

    // Context variables.

    optix::Handle<optix::VariableObj> operator[](const std::string& variable_name);
    optix::Handle<optix::VariableObj> operator[](const char* variable_name);

    // Validate and launch.

    void validate();

    enum EntryPoint
    {
        RaycastEntryPoint = 0,
        NumEntryPoints
    };

    void launch(const EntryPoint entry_point, const size_t size);

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_GPU_OPTIXCONTEXT_H
