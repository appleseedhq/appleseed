// Interface header.
#include "sourceshadingpoint.h"

namespace renderer
{
    SourceShadingPoint::SourceShadingPoint(const foundation::Vector2f uv)
        : uvx(uv.x)
        , uvy(uv.y)
        , pointx(0)
        , pointy(0)
        , pointz(0)
    {
    }
}