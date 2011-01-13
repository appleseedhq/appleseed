
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/bsp.h"
#include "foundation/utility/lazy.h"

// Standard headers.
#include <map>
#include <vector>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Leaf of a triangle tree.
//

typedef foundation::uint32 TriangleLeaf;


//
// Triangle tree.
//

class TriangleTree
  : public foundation::bsp::Tree<GScalar, 3, TriangleLeaf>
{
  public:
    // Construction arguments.
    struct Arguments
    {
        const foundation::UniqueID      m_triangle_tree_uid;
        const GAABB3                    m_bbox;
        const Assembly&                 m_assembly;
        const RegionInfoVector          m_regions;

        // Constructor.
        Arguments(
            const foundation::UniqueID  triangle_tree_uid,
            const GAABB3&               bbox,
            const Assembly&             assembly,
            const RegionInfoVector&     regions);
    };

    // Constructor, builds the tree for a given set of regions.
    explicit TriangleTree(const Arguments& arguments);

    // Destructor.
    ~TriangleTree();

  private:
    const foundation::UniqueID          m_triangle_tree_uid;
    std::vector<foundation::uint32*>    m_leaf_page_array;
};


//
// Triangle tree factory.
//

class TriangleTreeFactory
  : public foundation::ILazyFactory<TriangleTree>
{
  public:
    // Constructor.
    explicit TriangleTreeFactory(
        const TriangleTree::Arguments& arguments);

    // Create the triangle tree.
    virtual std::auto_ptr<TriangleTree> create();

  private:
    TriangleTree::Arguments m_arguments;
};


//
// Some additional types.
//

// Triangle tree container and iterator types.
typedef std::map<
            foundation::UniqueID,
            foundation::Lazy<TriangleTree>*
        > TriangleTreeContainer;
typedef TriangleTreeContainer::iterator TriangleTreeIterator;
typedef TriangleTreeContainer::const_iterator TriangleTreeConstIterator;

// Triangle tree access cache type.
typedef foundation::AccessCacheMap<
            TriangleTreeContainer,
            TriangleTreeAccessCacheSize
        > TriangleTreeAccessCache;


//
// Utility class to load a triangle from a given memory address,
// converting the triangle to the desired precision if necessary,
// but avoiding any work (in particular, no copy) if the triangle
// is stored with the desired precision and can be used in-place.
//

namespace impl
{
    template <bool CompatibleTypes>
    struct TriangleGeometryReader;

    // The triangle is stored with a different precision than the
    // required one, perform a conversion.
    template <>
    struct TriangleGeometryReader<false>
    {
        const TriangleType m_triangle;

        explicit TriangleGeometryReader(const GTriangleType& triangle)
          : m_triangle(triangle)
        {
        }
        explicit TriangleGeometryReader(const foundation::uint32* ptr)
          : m_triangle(*reinterpret_cast<const GTriangleType*>(ptr))
        {
        }
    };

    // The triangle is stored with the same precision as required,
    // use it directly, without any conversion or copy.
    template <>
    struct TriangleGeometryReader<true>
    {
        const TriangleType& m_triangle;

        explicit TriangleGeometryReader(const TriangleType& triangle)
          : m_triangle(triangle)
        {
        }
        explicit TriangleGeometryReader(const foundation::uint32* ptr)
          : m_triangle(*reinterpret_cast<const TriangleType*>(ptr))
        {
        }
    };
}

typedef impl::TriangleGeometryReader<sizeof(GScalar) == sizeof(double)> TriangleGeometryReader;


//
// Triangle leaf visitor, used during tree intersection.
//

class TriangleLeafVisitor
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit TriangleLeafVisitor(ShadingPoint& shading_point);

    // Visit a leaf.
    double visit(
        const TriangleLeaf*             leaf,
        const ShadingRay::RayType&      ray,
        const ShadingRay::RayInfoType&  ray_info);

    // Read additional data about the triangle that was hit, if any.
    void read_hit_triangle_data() const;

  private:
    ShadingPoint&               m_shading_point;
    const foundation::uint32*   m_triangle_ptr;
    size_t                      m_cold_data_index;
};


//
// Triangle leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class TriangleLeafProbeVisitor
  : public ProbeVisitorBase
{
  public:
    // Visit a leaf.
    double visit(
        const TriangleLeaf*             leaf,
        const ShadingRay::RayType&      ray,
        const ShadingRay::RayInfoType&  ray_info);
};


//
// Triangle tree intersectors.
//

typedef foundation::bsp::Intersector<
    double,
    TriangleTree,
    TriangleLeafVisitor
> TriangleLeafIntersector;

typedef foundation::bsp::Intersector<
    double,
    TriangleTree,
    TriangleLeafProbeVisitor
> TriangleLeafProbeIntersector;


//
// TriangleLeafVisitor class implementation.
//

// Constructor.
inline TriangleLeafVisitor::TriangleLeafVisitor(ShadingPoint& shading_point)
  : m_shading_point(shading_point)
  , m_triangle_ptr(0)
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H
