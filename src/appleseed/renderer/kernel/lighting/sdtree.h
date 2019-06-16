#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/sampling/mappings.h"

// Standard headers.
#include <array>

namespace renderer {

enum class ESpatialFilter {
    ENearest,
    EStochasticBox,
    EBox,
};

enum class EDirectionalFilter
{
    ENearest,
    EBox,
};

class DTreeSample
{
  public:
    foundation::Vector3f m_dir;
    float m_probability;
};

class DTreeWrapper
{
  public:
    class DTreeRecord
    {};

    void record(
        const DTreeRecord&           rec,
        EDirectionalFilter           directionalFilter);

    float pdf(
        const foundation::Vector3f&  dir) const;

    void sample(
        SamplingContext&             sampling_context,
        DTreeSample&                 sample);

};

class STree
{
  public:
    DTreeWrapper* get_d_tree_wrapper(const foundation::Vector3d& point);

  private:
    DTreeWrapper m_d_tree_wrapper;
};

struct GPTVertex
{
    DTreeWrapper *dTree;
    foundation::Vector3f dTreeVoxelSize;
    foundation::Vector3f point;
    foundation::Vector3f dir;

    renderer::Spectrum throughput;
    renderer::Spectrum bsdfVal;
    renderer::Spectrum radiance;

    float woPdf, bsdfPdf, dTreePdf;
    bool isDelta;

    void add_radiance(const renderer::Spectrum& radiance);
    
    void record_to_tree(STree&                  sd_tree,
                        float                   statistical_weight,
                        ESpatialFilter          spatial_filter,
                        EDirectionalFilter      directional_filter,
                        SamplingContext&        sampling_context);
};

class GPTVertexPath
{
  public:
    GPTVertexPath();
    void add_vertex(const GPTVertex& vertex);
    void add_radiance(const renderer::Spectrum& radiance);

    void record_to_tree(STree&                  sd_tree,
                        float                   statistical_weight,
                        ESpatialFilter          spatial_filter,
                        EDirectionalFilter      directional_filter,
                        SamplingContext&        sampling_context);
    bool is_full() const;

  private:
    std::array<GPTVertex, 32> path;
    int index;
};

}