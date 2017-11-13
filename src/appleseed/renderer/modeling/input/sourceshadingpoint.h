#ifndef APPLESEED_RENDERER_MODELING_INPUT_SOURCESHADINGPOINT_H
#define APPLESEED_RENDERER_MODELING_INPUT_SOURCESHADINGPOINT_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace renderer
{
    class APPLESEED_DLLSYMBOL SourceShadingPoint
    {
      public:
        SourceShadingPoint(const foundation::Vector2f uv);
        float       uvx;           // texture coordinates from UV set #0
        float       uvy;
        double      pointx;        // world space intersection point
        double      pointy;
        double      pointz;
    };
}
#endif  // !APPLESEED_RENDERER_MODELING_INPUT_SOURCESHADINGPOINT_H
