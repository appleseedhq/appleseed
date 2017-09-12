
// Interface header.
#include "sourceentity.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;

namespace renderer
{

//
// SourceEntity class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
    const char* Model = "source_entity";
}

UniqueID SourceEntity::get_class_uid()
{
    return g_class_uid;
}

SourceEntity::SourceEntity(
    const char*         name,
    const ParamArray&   params,
    Source*             source)
      : Entity(g_class_uid, params)
      , m_source(source)
{
    set_name(name);

    const EntityDefMessageContext message_context("texture", this);

    const std::string color_space =
        m_params.get_required<std::string>(
            "color_space",
            "linear_rgb",
            make_vector("linear_rgb", "srgb", "ciexyz"),
            message_context);
    if (color_space == "linear_rgb")
        m_color_space = ColorSpaceLinearRGB;
    else if (color_space == "srgb")
        m_color_space = ColorSpaceSRGB;
    else m_color_space = ColorSpaceCIEXYZ;
}

void SourceEntity::release()
{
    delete this;
}

const char* SourceEntity::get_model() const
{
    return Model;
}

ColorSpace SourceEntity::get_color_space() const
{
    return m_color_space;
}

}   // namespace renderer
