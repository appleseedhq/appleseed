
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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
#include "curveobject.h"

// appleseed.foundation headers.
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <fstream>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveObject class implementation.
//

struct CurveObject::Impl
{
    RegionKit                   m_region_kit;
    mutable Lazy<RegionKit>     m_lazy_region_kit;
    std::vector<BezierCurve3d>  m_curves;
    vector<string>              m_material_slots;

    Impl()
      : m_lazy_region_kit(&m_region_kit)
    {       
    }

    GAABB3 compute_bounds() const
    {
        AABB3d ret(AABB3d::invalid());
        for(size_t i = 0; i < m_curves.size(); i++)
            ret.insert(m_curves[i].get_bounds());
        return ret;
    }

    // Read a custom curve file and load it in m_curves.
    void load_curve_file(const string& filename)
    {
        std::ifstream input;
        const float curve_width = 0.009f;

        try
        {
            input.open(filename.c_str());
            if (input.good())
            {
                Stopwatch<DefaultWallclockTimer> stopwatch;
                stopwatch.start();

                // First read number of curves and degree of curves.
                size_t num_curves;
                size_t num_ctrl_pts;
                input >> num_curves;
                input >> num_ctrl_pts;
                for (size_t curve = 0; curve < num_curves; curve++)
                {
                    std::vector<Vector3d> m_ctrl_points;
                    for (size_t pt = 0; pt < num_ctrl_pts; pt++)
                    {
                        Vector3d point;
                        input >> point.x >> point.y >> point.z;
                        m_ctrl_points.push_back(point);
                    }

                    Vector3d ctrl_pts[4] = {m_ctrl_points[0], m_ctrl_points[1], m_ctrl_points[2], m_ctrl_points[3]};
                    BezierCurve3d curve(ctrl_pts, curve_width);
                    m_curves.push_back(curve);                    
                }

                stopwatch.measure();

                RENDERER_LOG_INFO(
                    "loaded curve file : %s ( %s curves ) in %s",
                    filename.c_str(),
                    pretty_int(num_curves).c_str(),
                    pretty_time(stopwatch.get_seconds()).c_str());
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "Something happend while reading %s",
                    filename.c_str());                
            }        
            input.close();
        }
        catch(std::exception& e)
        {
            std::cout<<e.what()<<"\n";
        }
    }
};

CurveObject::CurveObject(
    const char* name,
    const ParamArray& params)
  : Object(name, params)  
  , impl(new Impl())
{
    impl->load_curve_file(params.get("filename"));    
}

CurveObject::~CurveObject()
{
    delete impl;
}

void CurveObject::release()
{
    delete this;
}

const char* CurveObject::get_model() const
{
    return CurveObjectFactory::get_model();
}

GAABB3 CurveObject::compute_local_bbox() const
{
    return impl->compute_bounds();
}

Lazy<RegionKit>& CurveObject::get_region_kit()
{
    return impl->m_lazy_region_kit;
}

size_t CurveObject::get_material_slot_count() const
{
    return impl->m_material_slots.size();
}

const char* CurveObject::get_material_slot(const size_t index) const
{
    return impl->m_material_slots[index].c_str();
}

size_t CurveObject::get_curve_count() const
{
    return impl->m_curves.size();
}

const BezierCurve3d& CurveObject::get_curve(const size_t index) const
{
    assert(index < impl->m_curves.size());
    return impl->m_curves[index];
}

//
// CurveObjectFactory class implementation.
//

const char* CurveObjectFactory::get_model()
{
    return "curve_object";
}

auto_release_ptr<CurveObject> CurveObjectFactory::create(
    const char*       name,
    const ParamArray& params)
{
    return
        auto_release_ptr<CurveObject>(
            new CurveObject(name, params));        
}

}   // namespace renderer.
