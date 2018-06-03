
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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
#include "curveobjectwriter.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/curve/icurvewalker.h"
#include "foundation/curve/genericcurvefilewriter.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveObjectWriter class implementation.
//

namespace
{
    //
    // Curve object walker.
    //

    class CurveObjectWalker
            : public ICurveWalker
    {
    public:
        CurveObjectWalker(
                const CurveObject&   object)
                : m_object(object)
        {
            create_parameters();
        }

        size_t get_basis() const override
        {
            return static_cast<unsigned char>(m_object.get_basis());
        }

        size_t get_curve_count() const override
        {
            return m_curve_count;
        }

        size_t get_vertex_count(const size_t i) const override
        {
            return m_vertex_counts[i];
        }

        Vector3f get_vertex(const size_t i) const override
        {
            return m_vertices[i];
        }

        float get_vertex_width(const size_t i) const override
        {
            return m_widths[i];
        }

        float get_vertex_opacity(const size_t i) const override
        {
            return m_opacities[i];
        }

        Color3f get_vertex_colour(const size_t i) const override
        {
            return m_colors[i];
        }

    private:
        const CurveObject&  m_object;

        // Curve parameters for writing
        size_t              m_curve_count;
        vector<size_t>      m_vertex_counts;
        vector<GVector3>    m_vertices;
        vector<GScalar>     m_widths;
        vector<GScalar>     m_opacities;
        vector<GColor3>     m_colors;


        void create_curve1_parameters()
        {
            int32 vertex_count = 0;

            for (int32 i = 0; i < m_object.get_curve1_count(); i = i + 2)
            {
                if (!feq(m_vertices.back(), m_object.get_curve1(i).get_control_point(0)))
                {
                    m_vertices.push_back(m_object.get_curve1(i).get_control_point(0));
                    m_widths.push_back(m_object.get_curve1(i).get_width(0));
                    m_opacities.push_back(m_object.get_curve1(i).get_opacity(0));
                    m_colors.push_back(m_object.get_curve1(i).get_color(0));

                    m_vertex_counts.push_back(vertex_count);
                    vertex_count = 1;
                    m_curve_count++;
                }

                m_vertices.push_back(m_object.get_curve1(i).get_control_point(1));
                m_widths.push_back(m_object.get_curve1(i).get_width(1));
                m_opacities.push_back(m_object.get_curve1(i).get_opacity(1));
                m_colors.push_back(m_object.get_curve1(i).get_color(1));

                vertex_count++;
            }
        }

        void create_curve3_parameters()
        {
            int32 vertex_count = 0;

            for (int32 i = 0; i < m_object.get_curve1_count(); i = i + 4)
            {
                if (!feq(m_vertices.back(), m_object.get_curve3(i).get_control_point(0)))
                {
                    m_vertices.push_back(m_object.get_curve3(i).get_control_point(0));
                    m_widths.push_back(m_object.get_curve3(i).get_width(0));
                    m_opacities.push_back(m_object.get_curve3(i).get_opacity(0));
                    m_colors.push_back(m_object.get_curve3(i).get_color(0));

                    m_vertex_counts.push_back(vertex_count);
                    vertex_count = 1;
                    m_curve_count++;
                }

                m_vertices.push_back(m_object.get_curve3(i).get_control_point(1));
                m_widths.push_back(m_object.get_curve3(i).get_width(1));
                m_opacities.push_back(m_object.get_curve3(i).get_opacity(1));
                m_colors.push_back(m_object.get_curve3(i).get_color(1));

                vertex_count++;
            }
        }


        void create_parameters()
        {
            switch(m_object.get_basis())
            {
                case CurveBasis::LINEAR:
                    create_curve1_parameters();
                    break;

                case CurveBasis::BEZIER:
                case CurveBasis::BSPLINE:
                case CurveBasis::CATMULLROM:
                    create_curve3_parameters();
                    break;
            }
        }
    };
}

bool CurveObjectWriter::write(
    const CurveObject&  object,
    const char*         filepath)
{
    assert(filepath);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        GenericCurveFileWriter writer(filepath);
        CurveObjectWalker walker(object);
        writer.write(walker);
    }
    catch (const ExceptionIOError&)
    {
        RENDERER_LOG_ERROR(
                "failed to write curve file %s: i/o error.",
                filepath);
        return false;
    }
    catch (const Exception& e)
    {
        RENDERER_LOG_ERROR(
                "failed to write curve file %s: %s.",
                filepath,
                e.what());
        return false;
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
            "wrote curve file %s in %s.",
            filepath,
            pretty_time(stopwatch.get_seconds()).c_str());

    return true;
}

}   // namespace renderer
