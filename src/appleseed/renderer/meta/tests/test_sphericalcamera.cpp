
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/camera/sphericalcamera.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/entity/onrenderbeginrecorder.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Camera_SphericalCamera)
{
    TEST_CASE(ProjectPoint)
    {
        auto_release_ptr<Scene> scene(SceneFactory::create());
        scene->cameras().insert(SphericalCameraFactory().create("camera", ParamArray()));

        auto_release_ptr<Project> project(ProjectFactory::create("test"));
        project->set_scene(scene);
        project->set_frame(
            FrameFactory::create(
                "frame",
                ParamArray()
                    .insert("resolution", "512 512")
                    .insert("camera", "camera")));

        OnRenderBeginRecorder render_begin_recorder;
        bool success = project->get_scene()->on_render_begin(project.ref(), nullptr, render_begin_recorder);
        ASSERT_TRUE(success);

        OnFrameBeginRecorder frame_begin_recorder;
        success = project->get_scene()->on_frame_begin(project.ref(), nullptr, frame_begin_recorder);
        ASSERT_TRUE(success);

        const Camera* camera = project->get_scene()->get_render_data().m_active_camera;

        SamplingContext::RNGType rng;
        SamplingContext sampling_context(rng, SamplingContext::QMCMode);

        ShadingRay ray;
        camera->spawn_ray(sampling_context, Dual2d(Vector2d(1.0, 1.0)), ray);

        const Vector3d hit_point = ray.m_org + 3.0 * normalize(ray.m_dir);

        Vector2d projected;
        success = camera->project_point(0.0f, hit_point, projected);

        ASSERT_TRUE(success);
        EXPECT_FEQ(Vector2d(1.0, 1.0), projected);

        frame_begin_recorder.on_frame_end(project.ref());
        render_begin_recorder.on_render_end(project.ref());
    }
}
