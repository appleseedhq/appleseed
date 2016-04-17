
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/matrix.h"
#include "foundation/math/vector.h"
#include "foundation/ui/cameracontroller.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_UI_CameraController)
{
    typedef CameraController<double> CameraControllerd;

    TEST_CASE(Tumble_GivenCameraAndTargetAtSamePosition_DoesNotChangeCameraPosition)
    {
        // Create a camera controller and configure it such that the camera
        // and the target share the same position (the origin).
        CameraControllerd controller;
        controller.set_target(Vector3d(0.0));

        // Try to tumble the camera.
        controller.begin_drag(CameraControllerd::Tumble, Vector2d(0.0));
        controller.update_drag(Vector2d(1.0, 0.0));
        controller.end_drag();

        // The position of the camera shouldn't have changed.
        const CameraControllerd::MatrixType m = controller.get_transform();
        EXPECT_EQ(0.0, m[3]);
        EXPECT_EQ(0.0, m[7]);
        EXPECT_EQ(0.0, m[11]);
    }
}
