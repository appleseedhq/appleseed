
#ifndef APPLESEED_RENDERER_MODELING_SCENEENTITY_TEXTURE_H
#define APPLESEED_RENDERER_MODELING_SCENEENTITY_TEXTURE_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace renderer      { class ParamArray; }
namespace renderer      { class Source; }

namespace renderer
{

//
// The base class for textures.
//

class APPLESEED_DLLSYMBOL SourceEntity
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    SourceEntity(
        const char*         name,
        const ParamArray&   params,
        Source*             source);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const;

    // Return the color space of the texture.
    virtual foundation::ColorSpace get_color_space() const;

    void release() override;

    Source* m_source;

private:
    foundation::ColorSpace  m_color_space;

};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENEENTITY_TEXTURE_H
