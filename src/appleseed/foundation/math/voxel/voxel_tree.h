
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/split.h"
#include "foundation/math/vector.h"
#include "foundation/math/voxel/voxel_node.h"
#include "foundation/platform/types.h"
#include "foundation/utility/bufferedfile.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <string>
#include <vector>

namespace foundation {
namespace voxel {

//
// Voxel tree.
//

template <typename T, size_t N>
class Tree
{
  public:
    // Value type and dimension.
    typedef T ValueType;
    static const size_t Dimension = N;

    // Vector, AABB, node and tree types.
    typedef Vector<T, N> VectorType;
    typedef AABB<T, N> AABBType;
    typedef Node<T> NodeType;
    typedef Tree<T, N> TreeType;

    // Constructor.
    Tree();

    // Clear the tree.
    void clear();

    // Return the bounding box of the tree.
    const AABBType& get_bbox() const;

    // Return the maximum leaf node diagonal length.
    ValueType get_max_diag_length() const;

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    // Dump all solid leaves of the tree to disk, as an .obj mesh file.
    // Return true on success, false on error.
    bool dump_solid_leaves_to_disk(const std::string& filename) const;

    // Dump the entire tree to disk, in proprietary binary format.
    bool dump_tree_to_disk(const std::string& filename) const;

  protected:
    template <
        typename Tree,
        typename Timer
    >
    friend class Builder;

    template <
        typename T_,
        typename Tree,
        size_t S
    >
    friend class Intersector;

    template <typename Tree, typename Builder>
    friend class TreeStatistics;

    typedef std::vector<NodeType> NodeVector;

    AABBType    m_bbox;                     // bounding box of the tree
    NodeVector  m_nodes;                    // nodes of the tree
    ValueType   m_max_diag;                 // maximum leaf node diagonal length

    // Write a vertex definition to a file.
    static size_t dump_vertex(
        const double    x,
        const double    y,
        const double    z,
        std::FILE*      file);

    // Write a face definition to a file.
    static size_t dump_face(
        const size_t    base,
        const size_t    v0,
        const size_t    v1,
        const size_t    v2,
        const size_t    v3,
        std::FILE*      file);

    // Recursively write solid leaves to a file.
    bool dump_recurse(
        const size_t    node_index,
        const AABBType& node_bbox,
        size_t&         vertex_count,
        std::FILE*      file) const;
};


//
// Tree class implementation.
//

template <typename T, size_t N>
Tree<T, N>::Tree()
{
    clear();
}

template <typename T, size_t N>
void Tree<T, N>::clear()
{
    m_bbox.invalidate();
    m_nodes.clear();
    m_max_diag = ValueType(0.0);
}

template <typename T, size_t N>
inline const AABB<T, N>& Tree<T, N>::get_bbox() const
{
    return m_bbox;
}

template <typename T, size_t N>
inline T Tree<T, N>::get_max_diag_length() const
{
    return m_max_diag;
}

template <typename T, size_t N>
size_t Tree<T, N>::get_memory_size() const
{
    size_t mem_size = sizeof(*this);
    mem_size += m_nodes.capacity() * sizeof(NodeType);
    return mem_size;
}

template <typename T, size_t N>
bool Tree<T, N>::dump_solid_leaves_to_disk(const std::string& filename) const
{
    // Open the file for writing.
    // todo: switch to use foundation::BufferedFile.
    std::FILE* file = std::fopen(filename.c_str(), "wt");
    if (file == nullptr)
        return false;

    // Recursively dump the tree to disk.
    size_t vertex_count = 0;
    const bool result =
        dump_recurse(
            0,                              // root node
            m_bbox,                         // bounding box of the root node
            vertex_count,
            file);

    // Close the file.
    std::fclose(file);

    return result;
}

template <typename T, size_t N>
bool Tree<T, N>::dump_tree_to_disk(const std::string& filename) const
{
    // Open the file for writing.
    BufferedFile file(
        filename.c_str(),
        BufferedFile::BinaryType,
        BufferedFile::WriteMode);
    if (!file.is_open())
        return false;

    // Write the bounding box of the tree.
    file.write(m_bbox.min);
    file.write(m_bbox.max);

    // Write the nodes.
    const size_t node_count = m_nodes.size();
    file.write(node_count);
    for (size_t i = 0; i < node_count; ++i)
    {
        const NodeType& node = m_nodes[i];
        file.write(node.m_info);
        file.write(node.m_abscissa);
    }

    return true;
}

template <typename T, size_t N>
size_t Tree<T, N>::dump_vertex(
    const double    x,
    const double    y,
    const double    z,
    std::FILE*      file)
{
    return std::fprintf(file, "v %f %f %f\n", x, y, z) < 0 ? 0 : 1;
}

template <typename T, size_t N>
size_t Tree<T, N>::dump_face(
    const size_t    base,
    const size_t    v0,
    const size_t    v1,
    const size_t    v2,
    const size_t    v3,
    std::FILE*      file)
{
    return
        std::fprintf(
            file,
            "f " FMT_SIZE_T " " FMT_SIZE_T " " FMT_SIZE_T " " FMT_SIZE_T "\n",
            base + v0,
            base + v1,
            base + v2,
            base + v3) < 0 ? 0 : 1;
}

template <typename T, size_t N>
bool Tree<T, N>::dump_recurse(
    const size_t        node_index,
    const AABBType&     node_bbox,
    size_t&             vertex_count,
    std::FILE*          file) const
{
    // Fetch the node.
    const NodeType& node = m_nodes[node_index];

    if (node.is_leaf())
    {
        size_t result = 1;

        if (node.is_solid())
        {
            // Write the vertices of the node bounding box.
            result &= dump_vertex(node_bbox[0].x, node_bbox[0].y, node_bbox[0].z, file);
            result &= dump_vertex(node_bbox[0].x, node_bbox[0].y, node_bbox[1].z, file);
            result &= dump_vertex(node_bbox[0].x, node_bbox[1].y, node_bbox[1].z, file);
            result &= dump_vertex(node_bbox[0].x, node_bbox[1].y, node_bbox[0].z, file);
            result &= dump_vertex(node_bbox[1].x, node_bbox[0].y, node_bbox[0].z, file);
            result &= dump_vertex(node_bbox[1].x, node_bbox[0].y, node_bbox[1].z, file);
            result &= dump_vertex(node_bbox[1].x, node_bbox[1].y, node_bbox[1].z, file);
            result &= dump_vertex(node_bbox[1].x, node_bbox[1].y, node_bbox[0].z, file);

            // Write the faces of the node bounding box.
            result &= dump_face(vertex_count, 1, 2, 3, 4, file);
            result &= dump_face(vertex_count, 2, 6, 7, 3, file);
            result &= dump_face(vertex_count, 6, 5, 8, 7, file);
            result &= dump_face(vertex_count, 5, 1, 4, 8, file);
            result &= dump_face(vertex_count, 3, 7, 8, 4, file);
            result &= dump_face(vertex_count, 1, 5, 6, 2, file);
            vertex_count += 8;
        }

        return result == 1;
    }
    else
    {
        // Retrieve the splitting dimension and abscissa.
        const Split<ValueType> split(node.get_split_dim(), node.get_split_abs());

        // Compute the bounding boxes of the child nodes.
        AABBType left_node_bbox, right_node_bbox;
        split_bbox(node_bbox, split, left_node_bbox, right_node_bbox);

        // Visit the left subtree.
        if (!dump_recurse(
                node.get_child_node_index(),
                left_node_bbox,
                vertex_count,
                file))
                return false;

        // Visit the right subtree.
        return
            dump_recurse(
                node.get_child_node_index() + 1,
                right_node_bbox,
                vertex_count,
                file);
    }
}

}   // namespace voxel
}   // namespace foundation
