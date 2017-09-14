#ifndef APPLESEED_RENDERER_MODELING_INPUT_MAXSOURCE_H
#define APPLESEED_RENDERER_MODELING_INPUT_MAXSOURCE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/input/source.h"

// Standard headers.
#include <cstddef>
#include <vector>

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace renderer
{
    class APPLESEED_DLLSYMBOL MaxSource
        : public Source
    {
    public:
        // Constructor.
        MaxSource();

        // Destructor.
        virtual ~MaxSource();

        virtual foundation::uint64 compute_signature() const = 0;

        // Evaluate the source at a given shading point.
        virtual void evaluate(
            TextureCache&                       texture_cache,
            const foundation::Vector2f&         uv,
            float&                              scalar) const override;
        virtual void evaluate(
            TextureCache&                       texture_cache,
            const foundation::Vector2f&         uv,
            foundation::Color3f&                linear_rgb) const override;
        virtual void evaluate(
            TextureCache&                       texture_cache,
            const foundation::Vector2f&         uv,
            Spectrum&                           spectrum) const override;
        virtual void evaluate(
            TextureCache&                       texture_cache,
            const foundation::Vector2f&         uv,
            Alpha&                              alpha) const override;
        virtual void evaluate(
            TextureCache&                       texture_cache,
            const foundation::Vector2f&         uv,
            foundation::Color3f&                linear_rgb,
            Alpha&                              alpha) const override;
        virtual void evaluate(
            TextureCache&                       texture_cache,
            const foundation::Vector2f&         uv,
            Spectrum&                           spectrum,
            Alpha&                              alpha) const override;

        // Sample the texture. Return a color in the linear RGB color space.
        virtual foundation::Color4f sample_max_texture(
            const foundation::Vector2f&         uv) const;

    private:
        // Compute an alpha value given a linear RGBA color and the alpha mode of the texture instance.
        void evaluate_alpha(
            const foundation::Color4f&          color,
            Alpha&                              alpha) const;
    };

    //
    // MaxSource class implementation.
    //
    
    // Constructor.
    inline MaxSource::MaxSource()
        : Source(false)
    {
    }

    inline MaxSource::~MaxSource()
    {
    }

    inline void MaxSource::evaluate(
        TextureCache&                           texture_cache,
        const foundation::Vector2f&             uv,
        float&                                  scalar) const
    {
        const foundation::Color4f color = sample_max_texture(uv);
        scalar = color[0];
    }

    inline void MaxSource::evaluate(
        TextureCache&                           texture_cache,
        const foundation::Vector2f&             uv,
        foundation::Color3f&                    linear_rgb) const
    {
        const foundation::Color4f color = sample_max_texture(uv);
        linear_rgb = color.rgb();
    }

    inline void MaxSource::evaluate(
        TextureCache&                           texture_cache,
        const foundation::Vector2f&             uv,
        Spectrum&                               spectrum) const
    {
        const foundation::Color4f color = sample_max_texture(uv);
        spectrum.set(color.rgb(), g_std_lighting_conditions, Spectrum::Reflectance);
    }

    inline void MaxSource::evaluate(
        TextureCache&                           texture_cache,
        const foundation::Vector2f&             uv,
        Alpha&                                  alpha) const
    {
        const foundation::Color4f color = sample_max_texture(uv);
        evaluate_alpha(color, alpha);
    }

    inline void MaxSource::evaluate(
        TextureCache&                           texture_cache,
        const foundation::Vector2f&             uv,
        foundation::Color3f&                    linear_rgb,
        Alpha&                                  alpha) const
    {
        const foundation::Color4f color = sample_max_texture(uv);
        linear_rgb = color.rgb();
        evaluate_alpha(color, alpha);
    }

    inline void MaxSource::evaluate(
        TextureCache&                           texture_cache,
        const foundation::Vector2f&             uv,
        Spectrum&                               spectrum,
        Alpha&                                  alpha) const
    {
        const foundation::Color4f color = sample_max_texture(uv);
        spectrum.set(color.rgb(), g_std_lighting_conditions, Spectrum::Reflectance);
        evaluate_alpha(color, alpha);
    }

    inline void MaxSource::evaluate_alpha(
        const foundation::Color4f&              color,
        Alpha&                                  alpha) const
    {
    }

    inline foundation::Color4f MaxSource::sample_max_texture(
        const foundation::Vector2f& uv) const
    {
        return foundation::Color4f(1.0, 0.0, 1.0, 1.0); //return purple if not overriden
    }
}   // namespace renderer

#endif //APPLESEED_RENDERER_MODELING_INPUT_MAXSOURCE_H
