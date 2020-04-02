
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
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <exception>
#include <string>

using namespace foundation;
namespace bf = boost::filesystem;

namespace renderer
{

//
// CurveObjectReader class implementation.
//

namespace
{
    class CurveObjectBuilder
      : public ICurveBuilder
    {
      public:
        CurveObjectBuilder(
            const ParamArray& params,
            const std::string& name)
          : m_params(params)
          , m_name(name)
          , m_split_count(0)
          , m_total_vertex_count(0)
        {
        }

        CurveObject* get_object() const
        {
            return m_object;
        }

        size_t get_total_vertex_count() const
        {
            return m_total_vertex_count;
        }

        void begin_curve_object(const CurveBasis basis, const size_t count) override
        {
            // Create an empty curve object.
            m_object =
                static_cast<CurveObject*>(
                    CurveObjectFactory().create(m_name.c_str(), m_params).release());

            m_split_count = m_params.get_optional<size_t>("presplits", 3);

            m_object->push_basis(basis);
            m_object->push_curve_count(count);

            switch (static_cast<CurveBasis>(basis))
            {
              case CurveBasis::Linear:
                m_object->reserve_curves1(count);
                m_object->reserve_curves3(0);
                break;

              case CurveBasis::Bezier:
              case CurveBasis::BSpline:
              case CurveBasis::CatmullRom:
                m_object->reserve_curves3(count);
                m_object->reserve_curves1(0);
                break;

              assert_otherwise;
            }
        }

        void begin_curve() override
        {
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

              case CurveBasis::BSpline:
              case CurveBasis::CatmullRom:
                push_curve3(1);
                break;

              assert_otherwise;
            }

            m_total_vertex_count += m_vertices.size();
        }

        void end_curve_object() override
        {
        }

        void push_vertex(const Vector3f& v) override
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

        void push_vertex_color(const Color3f& c) override
        {
            return m_colors.push_back(c);
        }

        auto_release_ptr<CurveObject> create_hair_ball()
        {
            const size_t ControlPointCount = 4;
            const size_t curve_count = m_params.get_optional<size_t>("curves", 100);
            const GScalar curve_width = m_params.get_optional<GScalar>("width", GScalar(0.002));

            // todo: does this code really work with any basis?
            const unsigned char basis = m_params.get_optional<unsigned char>("basis", 2);
            assert(basis >= 1 && basis <= 4);

            begin_curve_object(static_cast<CurveBasis>(basis), curve_count);

            MersenneTwister rng;

            for (size_t c = 0; c < curve_count; ++c)
            {
                begin_curve();

                for (size_t p = 0; p < ControlPointCount; ++p)
                {
                    // http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
                    const GScalar r = std::pow(GScalar(1.0) - rand2<GScalar>(rng), GScalar(1.0) / 3);
                    const GVector3 d = sample_sphere_uniform(rand_vector2<GVector2>(rng));
                    push_vertex(r * d);
                    push_vertex_width(curve_width);
                    push_vertex_color(Color3f(0.2f, 0.0f, 0.7f));   // default color
                    push_vertex_opacity(1.0f);                      // default opacity
                }

                end_curve();
            }

            end_curve_object();

            return auto_release_ptr<CurveObject>(m_object);
        }

        auto_release_ptr<CurveObject> create_furry_ball()
        {
            const size_t ControlPointCount = 4;
            const size_t curve_count = m_params.get_optional<size_t>("curves", 100);
            const GScalar curve_length = m_params.get_optional<GScalar>("length", GScalar(0.1));
            const GScalar length_fuzziness = m_params.get_optional<GScalar>("length_fuzziness", GScalar(0.3));
            const GScalar root_width = m_params.get_optional<GScalar>("root_width", GScalar(0.001));
            const GScalar tip_width = m_params.get_optional<GScalar>("tip_width", GScalar(0.0001));
            const GScalar curliness = m_params.get_optional<GScalar>("curliness", GScalar(0.5));

            // todo: does this code really work with any basis?
            const unsigned char basis = m_params.get_optional<unsigned char>("basis", 2);
            assert(basis >= 1 && basis <= 4);

            begin_curve_object(static_cast<CurveBasis>(basis), curve_count);

            MersenneTwister rng;

            for (size_t c = 0; c < curve_count; ++c)
            {
                begin_curve();

                static const size_t Bases[] = { 2 };
                const GVector2 s = hammersley_sequence<GScalar, 2>(Bases, curve_count, c);
                const GVector3 d = sample_sphere_uniform(s);

                push_vertex(d);
                push_vertex_width(root_width);
                push_vertex_color(Color3f(0.2f, 0.0f, 0.7f));       // default color
                push_vertex_opacity(1.0f);                          // default opacity

                const GScalar f = rand1(rng, -length_fuzziness, +length_fuzziness);
                const GScalar length = curve_length * (GScalar(1.0) + f);

                for (size_t p = 1; p < ControlPointCount; ++p)
                {
                    const GScalar r = static_cast<GScalar>(p) / (ControlPointCount - 1);
                    const GVector3 f = curliness * sample_sphere_uniform(rand_vector2<GVector2>(rng));
                    push_vertex(m_vertices[0] + length * (r * d + f));
                    push_vertex_width(lerp(root_width, tip_width, r));
                    push_vertex_color(Color3f(0.2f, 0.0f, 0.7f));   // default color
                    push_vertex_opacity(1.0f);                      // default opacity
                }

                end_curve();
            }

            end_curve_object();

            return auto_release_ptr<CurveObject>(m_object);
        }

      private:
        const ParamArray         m_params;
        const std::string        m_name;
        CurveObject*             m_object;
        size_t                   m_split_count;

        // Curve attributes and statistics.
        std::vector<GVector3>    m_vertices;
        std::vector<GScalar>     m_widths;
        std::vector<GScalar>     m_opacities;
        std::vector<Color3f>     m_colors;

        // Global statistics.
        size_t              m_total_vertex_count;

        void reset_curve_variables()
        {
            m_vertices.clear();
            m_widths.clear();
            m_opacities.clear();
            m_colors.clear();
        }

        void split_and_store(const Curve3Type& curve, const size_t split_count)
        {
            if (split_count > 0)
            {
                Curve3Type child1, child2;
                curve.split(child1, child2);
                split_and_store(child1, split_count - 1);
                split_and_store(child2, split_count - 1);
            }
            else m_object->push_curve3(curve);
        }

        void push_curve1()
        {
            assert(m_vertices.size() >= 2);

            for (size_t i = 0; i < m_vertices.size() - 1; ++i)
            {
                const GVector3 points[2] = { m_vertices[i], m_vertices[i + 1] };
                const GScalar widths[2] = { m_widths[i], m_widths[i + 1] };
                const GScalar opacities[2] = { m_opacities[i], m_opacities[i + 1] };
                const Color3f colors[2] = { m_colors[i], m_colors[i + 1] };

                m_object->push_curve1(Curve1Type(points, widths, opacities, colors));
            }
        }

        void push_curve3(const size_t stride)
        {
            assert(m_vertices.size() >= 4);

            for (size_t i = 0; i < m_vertices.size() - 3; i += stride)
            {
                const GVector3 points[4] = { m_vertices[i], m_vertices[i + 1], m_vertices[i + 2], m_vertices[i + 3] };
                const GScalar widths[4] = { m_widths[i], m_widths[i + 1], m_widths[i + 2], m_widths[i + 3] };
                const GScalar opacities[4] = { m_opacities[i], m_opacities[i + 1], m_opacities[i + 2], m_opacities[i + 3] };
                const Color3f colors[4] = { m_colors[i], m_colors[i + 1], m_colors[i + 2], m_colors[i + 3] };

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
    CurveObjectBuilder builder(params, name);

    const std::string filepath = params.get<std::string>("filepath");

    if (filepath == "builtin:hairball")
        return builder.create_hair_ball();
    else if (filepath == "builtin:furryball")
        return builder.create_furry_ball();

    const size_t basis = params.get_optional<size_t>("basis", 2);
    if (basis < 1 || basis > 4)
    {
        RENDERER_LOG_ERROR("curves basis must be between 1 and 4 but was " FMT_SIZE_T ".", basis);
        return auto_release_ptr<CurveObject>(nullptr);
    }

    const float radius = params.get_optional<float>("radius", 0.01f);
    if (radius <= 0.0f)
    {
        RENDERER_LOG_ERROR("curves radius must be greater than zero.");
        return auto_release_ptr<CurveObject>(nullptr);
    }

    GenericCurveFileReader reader(
        search_paths.qualify(filepath.c_str()).c_str(),
        radius,
        basis);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        reader.read(builder);
    }
    catch (const std::exception& e)
    {
        RENDERER_LOG_ERROR("failed to load curve file %s: %s.", filepath.c_str(), e.what());
        return auto_release_ptr<CurveObject>(nullptr);
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
        "loaded curve file %s (%s %s, %s type, %s %s) in %s.",
        filepath.c_str(),
        pretty_uint(builder.get_object()->get_curve_count()).c_str(),
        builder.get_object()->get_curve_count() > 1 ? "curves" : "curve",
        get_curve_basis_name(builder.get_object()->get_basis()),
        pretty_uint(builder.get_total_vertex_count()).c_str(),
        builder.get_total_vertex_count() > 1 ? "vertices" : "vertex",
        pretty_time(stopwatch.get_seconds()).c_str());

    return auto_release_ptr<CurveObject>(builder.get_object());
}

}   // namespace renderer
