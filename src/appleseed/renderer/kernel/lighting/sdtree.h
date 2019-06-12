#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/sampling/mappings.h"

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

class STree {
  public:
    DTreeWrapper* get_dTreeWrapper();

  private:
    DTreeWrapper m_dTreeWrapper;
};

}