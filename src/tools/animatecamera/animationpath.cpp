
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

// Interface header.
#include "animationpath.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/log/log.h"

// Standard headers.
#include <cassert>
#include <cstdio>
#include <cstring>

using namespace foundation;

namespace appleseed {
namespace animatecamera {

AnimationPath::AnimationPath(Logger& logger)
  : m_logger(logger)
{
}

namespace
{
    class AutoClosingFile
      : public NonCopyable
    {
      public:
        explicit AutoClosingFile(const char* filename, const char* mode)
        {
            m_file = fopen(filename, mode);
        }

        ~AutoClosingFile()
        {
            if (m_file)
                fclose(m_file);
        }

        operator FILE*()
        {
            return m_file;
        }

      private:
        FILE* m_file;
    };

    Vector3d from_3ds_max(const Vector3d& v)
    {
        return Vector3d(v.x, v.z, -v.y);
    }
}

bool AnimationPath::load(const char* filename, const Format format)
{
    m_keyframes.clear();

    AutoClosingFile file(filename, "rt");

    if (file == nullptr)
    {
        LOG_ERROR(m_logger, "could not read animation path file %s.", filename);
        return false;
    }

    char header[1000];

    if (fgets(header, sizeof(header), file) == nullptr)
    {
        LOG_ERROR(m_logger, "could not read animation path file %s.", filename);
        return false;
    }

    if (strcmp(header, "frame pos_x pos_y pos_z rot_x rot_y rot_z rot_w\n"))
    {
        LOG_ERROR(m_logger, "%s is not a valid animation path file.", filename);
        return false;
    }

    int line = 2;

    while (!feof(file))
    {
        int frame;
        Vector3d position;
        Quaterniond orientation;

        const int fields_read =
            fscanf(
                file,
                "%d %lf %lf %lf %lf %lf %lf %lf\n",
                &frame,
                &position.x, &position.y, &position.z,
                &orientation.v.x, &orientation.v.y, &orientation.v.z, &orientation.s);

        if (fields_read != 8)
        {
            LOG_ERROR(
                m_logger,
                "while reading animation path file %s: at line %d: parse error.",
                filename,
                line);
            return false;
        }

        const int expected_frame = line - 2;

        if (frame != expected_frame)
        {
            LOG_ERROR(
                m_logger,
                "while reading animation path file %s: at line %d: expected frame %d, got frame %d.",
                filename,
                line,
                expected_frame,
                frame);
            return false;
        }

        if (!feq(norm(orientation), 1.0, 1.0e-4))
        {
            LOG_ERROR(
                m_logger,
                "while reading animation path file %s: at line %d: invalid rotation.",
                filename,
                line);
            return false;
        }

        orientation = normalize(orientation);

        if (format == Autodesk3dsMax)
        {
            position = from_3ds_max(position);
            orientation.v = from_3ds_max(orientation.v);

            m_keyframes.push_back(
                Transformd::from_local_to_parent(
                    Matrix4d::make_translation(position) *
                    Matrix4d::make_rotation(orientation) *
                    Matrix4d::make_rotation(Vector3d(1.0, 0.0, 0.0), -HalfPi<double>())));
        }
        else
        {
            m_keyframes.push_back(
                Transformd::from_local_to_parent(
                    Matrix4d::make_translation(position) *
                    Matrix4d::make_rotation(orientation)));
        }

        ++line;
    }

    LOG_INFO(
        m_logger,
        "read " FMT_SIZE_T " keyframe%s from animation path file %s.",
        m_keyframes.size(),
        m_keyframes.size() > 1 ? "s" : "",
        filename);

    return true;
}

size_t AnimationPath::size() const
{
    return m_keyframes.size();
}

const Transformd& AnimationPath::operator[](const size_t i) const
{
    assert(i < m_keyframes.size());
    return m_keyframes[i];
}

}   // namespace animatecamera
}   // namespace appleseed
