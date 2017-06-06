
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/lighttypes.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/utility/alignedvector.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/uid.h"

// Forward declarations.
namespace renderer      { class Scene; }
namespace renderer      { class NonPhysicalLightInfo; }
namespace renderer      { class EmittingTriangle; }

namespace renderer{

//
// Light tree.
//

class LightTree
  : public foundation::bvh::Tree<
               foundation::AlignedVector<
                   foundation::bvh::Node<foundation::AABB3d>
               >
            >
{
  public:
    // Constructor.
    // NOTE: Scene is not used at the moment as the lights are given as arguments
    //       Right now it is only a placeholder and reminder to use it later on.
    explicit LightTree(const Scene& scene);

    // Destructor
    ~LightTree();

    // Build the tree based on the lights collected by the LightSampler.
    // TODO: Remove light lists from arguments when they start being collected by the
    //       LightTree class itself.
    void build(
        const std::vector<NonPhysicalLightInfo>     non_physical_lights,
        const std::vector<EmittingTriangle>         emitting_triangles);

  private:
    struct Item
    {
        // foundation::UniqueID    m_light_uid;
        foundation::Vector3d    m_position;

        Item() {}

        Item(
            // const UniqueID          uid, //what to do with a triangle??
            foundation::AABB3d      bbox,
            foundation::Vector3d    position)
          ://non physicall light id - get_class_uid()
          m_position(position)
        {
        }
    };  

    typedef std::vector<EmittingTriangle>           EmittingTriangleVector;
    typedef std::vector<NonPhysicalLightInfo>       NonPhysicalLightVector;
    typedef std::vector<LightSource*>               LightSourcePointerVector;
    typedef std::vector<Item>                       ItemVector;

    LightSourcePointerVector   m_light_sources;
    ItemVector                 m_items;

    void store_items_in_leaves(foundation::Statistics& statistics);
};

}

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_H