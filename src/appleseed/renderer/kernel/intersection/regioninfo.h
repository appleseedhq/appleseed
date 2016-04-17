
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_REGIONINFO_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_REGIONINFO_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>
#include <vector>

namespace renderer
{

//
// The RegionInfo class uniquely identifies a region within an assembly.
//

class RegionInfo
{
  public:
    // Constructor.
    RegionInfo(
        const size_t    object_instance_index,
        const size_t    region_index,
        const GAABB3&   region_parent_bbox);

    // Return the index of the object instance within the assembly.
    size_t get_object_instance_index() const;

    // Return the index of the region within the region kit of the object.
    size_t get_region_index() const;

    // Return the parent space bounding box of the region.
    const GAABB3& get_region_parent_bbox() const;

  private:
    foundation::uint32  m_object_instance_index;
    foundation::uint32  m_region_index;
    GAABB3              m_region_parent_bbox;
};

// A vector of RegionInfo.
typedef std::vector<RegionInfo> RegionInfoVector;


//
// RegionInfo class implementation.
//

inline RegionInfo::RegionInfo(
    const size_t        object_instance_index,
    const size_t        region_index,
    const GAABB3&       region_parent_bbox)
  : m_object_instance_index(static_cast<foundation::uint32>(object_instance_index))
  , m_region_index(static_cast<foundation::uint32>(region_index))
  , m_region_parent_bbox(region_parent_bbox)
{
}

inline size_t RegionInfo::get_object_instance_index() const
{
    return static_cast<size_t>(m_object_instance_index);
}

inline size_t RegionInfo::get_region_index() const
{
    return static_cast<size_t>(m_region_index);
}

inline const GAABB3& RegionInfo::get_region_parent_bbox() const
{
    return m_region_parent_bbox;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_REGIONINFO_H
