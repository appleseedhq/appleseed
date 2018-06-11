
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
#include "curveobjectreader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include <foundation/core/exceptions/exceptionioerror.h>
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/curve/genericcurvefilereader.h"
#include "foundation/curve/icurvebuilder.h"
#include "foundation/curve/icurvefilereader.h"
#include "foundation/math/aabb.h"
#include "foundation/math/fp.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <fstream>
#include <string>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace renderer
{

//
// CurveObjectReader class implementation.
//

namespace {

    //
    // Curve object builder.
    //

    class CurveObjectBuilder
            : public ICurveBuilder {
    public:

        CurveObjectBuilder(
                const ParamArray &params,
                const string &name)
                : m_params(params), m_name(name), m_split_count(0), m_total_vertex_count(0)
        {
        }

        CurveObject* get_object()
        {
            return m_object;
        }

        size_t get_curve_count()
        {
            return m_object->get_curve_count();
        }

        size_t get_total_vertex_count()
        {
            return m_total_vertex_count;
        }

        string get_basis()
        {
            string basis;

            switch(m_object->get_basis())
            {
                case CurveBasis::Linear:
                    basis = "linear";
                    break;

                case CurveBasis::Bezier:
                    basis = "bezier";
                    break;

                case CurveBasis::Bspline:
                    basis = "b-spline";
                    break;

                case CurveBasis::Catmullrom:
                    basis = "catmull-rom";
                    break;
            }

            return basis;
        }

        void begin_curve_object(unsigned char basis, uint32 count = 0) override
        {
            // Create an empty curve object.
            m_object = static_cast<CurveObject *>(
                    CurveObjectFactory().create(m_name.c_str(), m_params).release());

            m_split_count = m_params.get_optional<size_t>("presplits", 0);

            m_object->push_basis(basis);
            m_object->push_curve_count(count);

            switch (static_cast<CurveBasis>(basis))
            {
                case CurveBasis::Linear:
                    m_object->reserve_curves1(count);
                    m_object->reserve_curves3(0);
                    break;

                case CurveBasis::Bezier:
                case CurveBasis::Bspline:
                case CurveBasis::Catmullrom:
                    m_object->reserve_curves3(count);
                    m_object->reserve_curves1(0);
                    break;
            }

        }

        void begin_curve() override {

            // Reset curve variables.
            reset_curve_variables();
        }

        void end_curve() override
        {
            switch (m_object->get_basis())
            {
                case CurveBasis::Linear:
                    push_curve1();
                    break;

                case CurveBasis::Bezier:
                    push_curve3(3);
                    break;

                case CurveBasis::Bspline:
                case CurveBasis::Catmullrom:
                    push_curve3(1);
                    break;
            }

            m_total_vertex_count += m_vertices.size();
        }

        void end_curve_object() override
        {

        }

        void push_vertex(const Vector3f &v) override
        {
            return m_vertices.push_back(GVector3(v));
        }

        void push_vertex_width(const float w) override
        {
            return m_widths.push_back(w);
        }

        void push_vertex_opacity(const float o) override
        {
            return m_opacities.push_back(o);
        }

        void push_vertex_color(const Color3f &c) override
        {
            return m_colors.push_back(GColor3(c));
        }


    private:
        const ParamArray    m_params;
        const string        m_name;
        CurveObject*        m_object;
        size_t              m_split_count;

        // Curve attributes and statistics
        vector<GVector3>    m_vertices;
        vector<GScalar>     m_widths;
        vector<GScalar>     m_opacities;
        vector<GColor3>     m_colors;

        // Global statistics
        size_t              m_total_vertex_count;


        void reset_curve_variables()
        {
            m_vertices.clear();
            m_widths.clear();
            m_opacities.clear();
            m_colors.clear();
        }


        void split_and_store(const Curve3Type &curve, const size_t split_count)
        {
            if (split_count > 0) {
                Curve3Type child1, child2;
                curve.split(child1, child2);
                split_and_store(child1, split_count - 1);
                split_and_store(child2, split_count - 1);
            } else m_object->push_curve3(curve);
        }

        void push_curve1() {
            for (int32 i = 0; i < m_vertices.size() - 1; ++i) {
                GVector3 points[2] = {m_vertices[i], m_vertices[i + 1]};
                GScalar widths[2] = {m_widths[i], m_widths[i + 1]};
                GScalar opacities[2] = {m_opacities[i], m_opacities[i + 1]};
                GColor3 colors[2] = {m_colors[i], m_colors[i + 1]};

                m_object->push_curve1(Curve1Type(points, widths, opacities, colors));
            }
        }

        void push_curve3(int stride)
        {
            for (int32 i = 0; i < m_vertices.size() - 3; i = i + stride) {
                GVector3 points[4] = {m_vertices[i], m_vertices[i + 1], m_vertices[i + 2], m_vertices[i + 3]};
                GScalar widths[4] = {m_widths[i], m_widths[i + 1], m_widths[i + 2], m_widths[i + 3]};
                GScalar opacities[4] = {m_opacities[i], m_opacities[i + 1], m_opacities[i + 2], m_opacities[i + 3]};
                GColor3 colors[4] = {m_colors[i], m_colors[i + 1], m_colors[i + 2], m_colors[i + 3]};

                split_and_store(Curve3Type(points, widths, opacities, colors), m_split_count);
            }
        }
    };
}


auto_release_ptr<CurveObject> CurveObjectReader::read(
    const SearchPaths&      search_paths,
    const char*             name,
    const ParamArray&       params)
{
    const string filepath = params.get<string>("filepath");

    const size_t degree = params.get_optional<size_t>("degree", 3);
    if (degree != 1 && degree != 3)
    {
        RENDERER_LOG_ERROR("curves degree must be 1 or 3 but was " FMT_SIZE_T ".", degree);
        return auto_release_ptr<CurveObject>(nullptr);
    }

    const float radius = params.get_optional<float>("radius", 0.01f);
    if (radius <= 0.0f)
    {
        RENDERER_LOG_ERROR("curves radius must be greater than zero.");
        return auto_release_ptr<CurveObject>(nullptr);
    }

    GenericCurveFileReader reader(search_paths.qualify(filepath.c_str()).c_str(), radius, degree);

    CurveObjectBuilder builder(params, name);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        reader.read(builder);
    }

    catch (const ExceptionIOError&)
    {
        RENDERER_LOG_ERROR(
                "failed to load curve file %s: i/o error.",
                filepath.c_str());

        return auto_release_ptr<CurveObject>(nullptr);
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR(
                "failed to load curve file %s: %s.",
                filepath.c_str(),
                e.what());

        return auto_release_ptr<CurveObject>(nullptr);
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
            "loaded curve file %s (%s %s, %s %s, %s %s) in %s.",
            filepath.c_str(),
            pretty_int(builder.get_curve_count()).c_str(),"curves",
            builder.get_basis().c_str(), "type",
            pretty_int(builder.get_total_vertex_count()).c_str(),
            builder.get_total_vertex_count() > 1 ? "vertices" : "vertex",
            pretty_time(stopwatch.get_seconds()).c_str());


    return auto_release_ptr<CurveObject>(builder.get_object());
}

}   // namespace renderer
