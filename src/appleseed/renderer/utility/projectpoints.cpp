
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

// Interface header.
#include "projectpoints.h"

// appleseed.renderer headers.
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/entity/onrenderbeginrecorder.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

struct ProjectPoints::Impl
{
    Camera*                     m_camera;
    bool                        m_initialized;
    auto_release_ptr<Project>   m_project;
    OnRenderBeginRecorder       m_render_begin_recorder;
    OnFrameBeginRecorder        m_frame_begin_recorder;

    Impl()
      : m_camera(nullptr)
      , m_initialized(false)
    {
    }
};

ProjectPoints::ProjectPoints(
    auto_release_ptr<Camera> camera,
    const Vector2u&          resolution)
  : impl(new Impl())
{
    auto_release_ptr<Scene> scene = SceneFactory::create();

    const std::string camera_name = camera->get_name();
    scene->cameras().insert(camera);

    impl->m_project = ProjectFactory::create("project_points");
    impl->m_project->set_scene(scene);

    impl->m_project->set_frame(
        FrameFactory::create(
            "frame",
            ParamArray()
                .insert("resolution", resolution)
                .insert("camera", camera_name.c_str())));

    impl->m_initialized =
        impl->m_project->get_scene()->on_render_begin(
            impl->m_project.ref(),
            nullptr,
            impl->m_render_begin_recorder);

    impl->m_initialized = impl->m_initialized &&
        impl->m_project->get_scene()->on_frame_begin(
            impl->m_project.ref(),
            nullptr,
            impl->m_frame_begin_recorder);

    if (impl->m_initialized)
        impl->m_camera = impl->m_project->get_scene()->get_render_data().m_active_camera;
}

ProjectPoints::~ProjectPoints()
{
    impl->m_frame_begin_recorder.on_frame_end(impl->m_project.ref());
    impl->m_render_begin_recorder.on_render_end(impl->m_project.ref());

    delete impl;
}

bool ProjectPoints::is_initialized() const
{
    return impl->m_initialized;
}

bool ProjectPoints::project_point(
    const float              time,
    const Vector3d&          point,
    Vector2d&                ndc) const
{
    assert(is_initialized());
    return impl->m_camera->project_point(time, point, ndc);
}

bool ProjectPoints::project_camera_space_point(
    const Vector3d&          point,
    Vector2d&                ndc) const
{
    assert(is_initialized());
    return impl->m_camera->project_camera_space_point(point, ndc);
}

}   // namespace renderer

