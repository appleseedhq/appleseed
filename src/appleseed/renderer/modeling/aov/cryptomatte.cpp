
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Sergo Pogosyan, The appleseedhq Organization
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

// Interface header.
#include "cryptomatte.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/string.h"

// Murmurhash3 headers.
#include "MurmurHash3.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace
{
    class MultiChannelExrFileWriter
        : public IImageFileWriter
    {
      public:
        explicit MultiChannelExrFileWriter(const char* filename, const ICanvas* image)
            : m_canvas(image)
            , m_filename(filename)
        {
            m_writer = OIIO::ImageOutput::create(m_filename);
            if (m_writer == nullptr)
            {
                const string msg = OIIO::geterror();
                throw ExceptionIOError(msg.c_str());
            }

            m_spec = OIIO::ImageSpec();
            set_image_spec();
        }

        MultiChannelExrFileWriter::~MultiChannelExrFileWriter()
        {
            // Destroy the ImageOutput stucture.
            if (m_writer != nullptr)
                OIIO::ImageOutput::destroy(m_writer);
        }

        void write()
        {
            if (!m_writer->supports("tiles"))
                return;

            if (!m_writer->open(m_filename, m_spec))
            {
                const string msg = m_writer->geterror();
                throw ExceptionIOError(msg.c_str());
            }

            // m_writer->write_image(OIIO::TypeDesc::FLOAT, m_canvas);
            write_tiles();
            close_file();
        }

        void set_image_attributes(const ImageAttributes& image_attributes)
        {
            for (const_each<ImageAttributes> i = image_attributes; i; ++i)
            {
                // Fetch the name and the value of the attribute.
                const string attr_name = i->key();
                const string attr_value = i->value<string>();

                if (attr_name == "software")
                    m_spec.attribute("Software", attr_value.c_str());

                else if (attr_name == "color_space")
                {
                    if (attr_value == "linear")
                        m_spec.attribute("oiio:ColorSpace", "Linear");
                    else
                        m_spec.attribute("oiio:ColorSpace", attr_value.c_str());
                }

                else if (attr_name == "dpi")
                {
                    const size_t dpi = from_string<size_t>(attr_value);
                    const float dpm = dpi * (100.0f / 2.54f);
                    m_spec.attribute("XResolution", dpm);
                    m_spec.attribute("YResolution", dpm);
                    m_spec.attribute("ResolutionUnit", "cm");
                }

                else
                    m_spec.attribute(attr_name, attr_value.c_str());
            }
        }

        void set_image_channels(
            const size_t            channel_count,
            const vector<string>&   channel_names)
        {
            m_spec.nchannels = static_cast<int>(channel_count);

            for (size_t i = 0; i < channel_count; i++)
            {
                m_spec.channelnames.push_back(channel_names[i]);
            }
        }

    private:
        void close_file()
        {
            // Close the image file.
            if (!m_writer->close())
            {
                const string msg = m_writer->geterror();
                throw ExceptionIOError(msg.c_str());
            }
        }

        void set_image_spec()
        {
            const CanvasProperties& props = m_canvas->properties();

            // Size of the data of the image.
            m_spec.width = static_cast<int>(props.m_canvas_width);
            m_spec.height = static_cast<int>(props.m_canvas_height);

            // Origin of the pixel data of the image.
            m_spec.x = 0;
            m_spec.y = 0;

            // Full size of the data of the image.
            m_spec.full_width = m_spec.width;
            m_spec.full_height = m_spec.height;

            // Origin of the pixel data of the full image.
            m_spec.full_x = m_spec.x;
            m_spec.full_y = m_spec.y;

            // Size of a tile.
            m_spec.tile_width = static_cast<int>(props.m_tile_width);
            m_spec.tile_height = static_cast<int>(props.m_tile_height);

            m_spec.set_format(OIIO::TypeDesc::FLOAT);
        }
        
        void write_tiles()
        {
            // Retrieve canvas properties.
            const CanvasProperties& props = m_canvas->properties();

            // Compute the tiles' xstride offset in bytes.
            size_t xstride = props.m_pixel_size;

            // Loop over the columns of tiles.
            for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
            {
                // Loop over the rows of tiles.
                for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
                {
                    // Compute the offset of the tile in pixels from the origin (origin: x=0;y=0).
                    const size_t tile_offset_x = tile_x * props.m_tile_width;
                    assert(tile_offset_x <= props.m_canvas_width);
                    const size_t tile_offset_y = tile_y * props.m_tile_height;
                    assert(tile_offset_y <= props.m_canvas_height);

                    // Compute the tile's ystride offset in bytes.
                    const size_t ystride = xstride * min(static_cast<size_t>(m_spec.width + m_spec.x - tile_offset_x),
                        static_cast<size_t>(m_spec.tile_width));

                    // Retrieve the (tile_x, tile_y) tile.
                    const Tile& tile = m_canvas->tile(tile_x, tile_y);

                    // Write the tile into the file.
                    if (!m_writer->write_tile(
                        static_cast<int>(tile_offset_x),
                        static_cast<int>(tile_offset_y),
                        0,
                        OIIO::TypeDesc::FLOAT,
                        tile.get_storage(),
                        xstride,
                        ystride))
                    {
                        const string msg = m_writer->geterror();
                        close_file();
                        throw ExceptionIOError(msg.c_str());
                    }
                }
            }
        }

    private:
        const ICanvas*      m_canvas;
        OIIO::ImageSpec     m_spec;
        OIIO::ImageOutput*  m_writer;
        const char*         m_filename;
    };

    float hash_to_float(uint32_t hash) {
        // if all exponent bits are 0 (subnormals, +zero, -zero) set exponent to 1
        // if all exponent bits are 1 (NaNs, +inf, -inf) set exponent to 254
        uint32_t exponent = hash >> 23 & 255; // extract exponent (8 bits)
        if (exponent == 0 || exponent == 255)
            hash ^= 1 << 23; // toggle bit
        float f;
        memcpy(&f, &hash, 4);
        return f;
    }
}

namespace renderer
{

namespace
{

    typedef map<uint32_t, string> NameMap;
    typedef map<uint32_t, float> WeightMap;

    //
    // Cryptomatte AOV accumulator.
    //

    class CryptomatteAOVAccumulator
      : public AOVAccumulator
    {
      public:
        CryptomatteAOVAccumulator(
            Image&                              aov_image,
            vector<WeightMap>&                  pixel_samples,
            map<pair<int, int>, NameMap>&       tile_name_map,
            int                                 num_layers,
            CryptomatteAOV::CryptomatteType     layer_type)
            : m_aov_image(aov_image)
            , m_pixel_samples(pixel_samples)
            , m_num_layers(num_layers)
            , m_tile_name_map(tile_name_map)
            , m_layer_type(layer_type)
        {
        }

        void on_tile_begin(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            const size_t                max_spp) override
        {
            // Fetch the destination tile.
            const CanvasProperties& props = frame.image().properties();
            const Tile& tile = frame.image().tile(tile_x, tile_y);

            m_frame_width = props.m_canvas_width;
            m_rcp_canvas_width = props.m_rcp_canvas_width;
            m_rcp_canvas_height = props.m_rcp_canvas_height;
            m_renderer_filter = &(frame.get_filter());

            // Fetch the tile bounds (inclusive).
            m_tile_origin_x = static_cast<int>(tile_x * props.m_tile_width);
            m_tile_origin_y = static_cast<int>(tile_y * props.m_tile_height);
            m_tile_end_x = static_cast<int>(m_tile_origin_x + tile.get_width() - 1);
            m_tile_end_y = static_cast<int>(m_tile_origin_y + tile.get_height() - 1);

            for (int ry = m_tile_origin_y; ry <= m_tile_end_y; ++ry)
            {
                for (int rx = m_tile_origin_x; rx <= m_tile_end_x; ++rx)
                {
                    m_pixel_samples[ry * m_frame_width + rx] = WeightMap();
                }
            }

            if (frame.has_crop_window())
                m_crop_window = frame.get_crop_window();
            else
                m_crop_window = AABB2u(Vector2u(m_tile_origin_x, m_tile_origin_y), Vector2u(m_tile_end_x, m_tile_end_y));
        }

        void on_tile_end(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y) override
        {
            for (int ry = m_tile_origin_y; ry <= m_tile_end_y; ++ry)
            {
                for (int rx = m_tile_origin_x; rx <= m_tile_end_x; ++rx)
                {
                    const WeightMap& weight_map = m_pixel_samples[ry * m_frame_width + rx];

                    if (weight_map.size() > 0)
                    {
                        float total_weight = 0.0f;
                        for (const auto& item : weight_map)
                            total_weight += item.second;

                        if (total_weight == 0.0f)
                            total_weight = 1.0f;

                        vector<pair<float, uint32_t>> ranked_vector;
                        for (const auto& item : weight_map)
                            ranked_vector.push_back(make_pair(item.second, item.first));
                        sort(ranked_vector.begin(), ranked_vector.end(),
                            [](pair<float, uint32_t> const& a, pair<float, uint32_t> const& b) { return a.first > b.first; });

                        uint32_t m3hash = ranked_vector[0].second;

                        vector<float> pixel_values;
                        pixel_values.push_back(m3hash == 0 ? 0.0f : hash_to_float(m3hash));
                        pixel_values.push_back(m3hash == 0 ? 0.0f : ((float)((m3hash << 8)) / (float)UINT32_MAX));
                        pixel_values.push_back(m3hash == 0 ? 0.0f : ((float)((m3hash << 16)) / (float)UINT32_MAX));

                        // Remove background contribution
                        if (ranked_vector.size() > 1 && m3hash == 0)
                            ranked_vector.erase(ranked_vector.begin());

                        for (size_t i = 0, e = ranked_vector.size(); i < e && i < m_num_layers; ++i)
                        {
                            m3hash = ranked_vector[i].second;
                            pixel_values.push_back(m3hash == 0 ? 0.0f : hash_to_float(m3hash));
                            pixel_values.push_back(m3hash == 0 ? 0.0f : ranked_vector[i].first / total_weight);
                        }

                        const int num_channels = (m_num_layers * 2) + 3;
                        for (size_t i = ranked_vector.size(); i < num_channels; ++i)
                            pixel_values.push_back(0.0f);

                        m_aov_image.set_pixel(rx, ry, pixel_values.data());
                    }
                }
            }
        }

        void on_sample_begin(
            const PixelContext&         pixel_context) override
        {
        }

        void on_sample_end(
            const PixelContext&         pixel_context) override
        {
        }

        void on_pixel_begin(
            const Vector2i&             pi) override
        {
        }

        void on_pixel_end(
            const Vector2i&             pi) override
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            uint32_t m3hash = 0;
            string obj_name;
            if (shading_point.hit_surface())
            {
                switch (m_layer_type)
                {
                  case CryptomatteAOV::CryptomatteType::ObjectNames:
                    obj_name = shading_point.get_object().get_name();
                    break;

                  case CryptomatteAOV::CryptomatteType::MaterialNames:
                    {
                      const auto* obj_material = shading_point.get_material();
                      if (obj_material != nullptr)
                          obj_name = obj_material->get_name();
                    }
                    break;

                  default:
                    break;
                }

                MurmurHash3_x86_32(reinterpret_cast<const unsigned char*>(obj_name.c_str()), static_cast<int>(obj_name.size()), 0, &m3hash);
            }

            const Vector2i pixel_pos = pixel_context.get_pixel_coords();
            const Vector2d sample_pos = pixel_context.get_sample_position();
            int tile_pixel_x = pixel_pos.x - m_tile_origin_x;
            int tile_pixel_y = pixel_pos.y - m_tile_origin_y;

            m_tile_name_map[make_pair(m_tile_origin_x, m_tile_origin_y)][m3hash] = obj_name;    //tile_origin indexing of the map should help avoiding the race condition

            double sample_pos_x = (sample_pos.x / m_rcp_canvas_width) - pixel_pos.x;
            double sample_pos_y = (sample_pos.y / m_rcp_canvas_height) - pixel_pos.y;

            AABB2u tile_crop_window(
                Vector2u(m_crop_window.min.x - m_tile_origin_x, m_crop_window.min.y - m_tile_origin_y),
                Vector2u(m_crop_window.max.x - m_tile_origin_x, m_crop_window.max.y - m_tile_origin_y)
            );
            
            filter(
                static_cast<float>(tile_pixel_x + sample_pos_x),
                static_cast<float>(tile_pixel_y + sample_pos_y),
                tile_crop_window,
                m3hash);
        }

    private:
        const Filter2f*                 m_renderer_filter;
        int                             m_tile_origin_x;
        int                             m_tile_origin_y;
        int                             m_tile_end_x;
        int                             m_tile_end_y;
        size_t                          m_frame_width;
        double                          m_rcp_canvas_width;
        double                          m_rcp_canvas_height;
        AABB2u                          m_crop_window;
        Image&                          m_aov_image;
        int                             m_num_layers;
        vector<WeightMap>&              m_pixel_samples;
        map<pair<int, int>, NameMap>&   m_tile_name_map;
        CryptomatteAOV::CryptomatteType m_layer_type;

        // Copy of the FilteredTile::add().
        void filter(
            const float                 x,
            const float                 y,
            const AABB2u&               crop_window,
            const uint32_t              m3hash)
        {
            assert(m_renderer_filter);

            // Convert (x, y) from continuous image space to discrete image space.
            const float dx = x - 0.5f;
            const float dy = y - 0.5f;

            // Find the pixels affected by this sample.
            AABB2i footprint;
            footprint.min.x = truncate<int>(fast_ceil(dx - m_renderer_filter->get_xradius()));
            footprint.min.y = truncate<int>(fast_ceil(dy - m_renderer_filter->get_yradius()));
            footprint.max.x = truncate<int>(fast_floor(dx + m_renderer_filter->get_xradius()));
            footprint.max.y = truncate<int>(fast_floor(dy + m_renderer_filter->get_yradius()));

            // Don't affect pixels outside the crop window.
            footprint = AABB2i::intersect(footprint, crop_window);

            // Bail out if the point does not fall inside the crop window.
            // Only check the x coordinate; the y coordinate is checked in the loop below.
            if (footprint.min.x > footprint.max.x)
                return;

            for (int ry = footprint.min.y; ry <= footprint.max.y; ++ry)
            {
                for (int rx = footprint.min.x; rx <= footprint.max.x; ++rx)
                {
                    const float weight = m_renderer_filter->evaluate(rx - dx, ry - dy);
                    m_pixel_samples[(m_tile_origin_y + ry) * m_frame_width + (m_tile_origin_x + rx)][m3hash] += weight; // possible race conditions writing to pixels on the tile edges
                }
            }
            return;
        }
    };

    const char* CryptomatteAOVModel = "cryptomatte_aov";
}


//
// Cryptomatte AOV class implementation.
//

struct CryptomatteAOV::Impl
{
    vector<WeightMap>                   m_pixel_samples;
    map<pair<int, int>, NameMap>        m_tile_name_map;
    unique_ptr<Image>                   m_image;
    int                                 m_num_layers;
    CryptomatteAOV::CryptomatteType     m_layer_type;
};

CryptomatteAOV::CryptomatteAOV(
    int num_layers,
    CryptomatteType layer_type)
  : AOV("cryptomatte", ParamArray())
  , impl(new Impl())
{
    impl->m_num_layers = num_layers;
    impl->m_layer_type = layer_type;
}

CryptomatteAOV::~CryptomatteAOV()
{
    delete impl;
}

const char* CryptomatteAOV::get_model() const
{
    return CryptomatteAOVModel;
}

size_t CryptomatteAOV::get_channel_count() const
{
    return 0;
}

const char** CryptomatteAOV::get_channel_names() const
{
    return nullptr;
}

bool CryptomatteAOV::has_color_data() const
{
    return false;
}

void CryptomatteAOV::create_image(
    const size_t    canvas_width,
    const size_t    canvas_height,
    const size_t    tile_width,
    const size_t    tile_height,
    ImageStack&     aov_images)
{
    const int w = static_cast<int>(canvas_width);
    const int h = static_cast<int>(canvas_height);

    // Create underlying images.
    const int num_channels = (impl->m_num_layers * 2) + 3;
    impl->m_image.reset(
        new Image(
            canvas_width,
            canvas_height,
            tile_width,
            tile_height,
            num_channels,
            PixelFormatFloat));
    impl->m_pixel_samples.resize(w * h);
    clear_image();
}

void CryptomatteAOV::clear_image()
{
    const int num_channels = (impl->m_num_layers * 2) + 3;
    for (size_t ry = 0, e = impl->m_image->properties().m_canvas_height; ry < e; ++ry)
    {
        for (size_t rx = 0, e = impl->m_image->properties().m_canvas_width; rx < e; ++rx)
        {
                vector<float> pixel_values;
                for (size_t i = 0; i < num_channels; ++i)
                    pixel_values.push_back(0.0f);

                impl->m_image->set_pixel(rx, ry, pixel_values.data());
        }
    }
    impl->m_tile_name_map.clear();
}

auto_release_ptr<AOVAccumulator> CryptomatteAOV::create_accumulator() const
{
    return auto_release_ptr<AOVAccumulator>(
        new CryptomatteAOVAccumulator(
            *impl->m_image,
            impl->m_pixel_samples,
            impl->m_tile_name_map,
            impl->m_num_layers,
            impl->m_layer_type));
}

bool CryptomatteAOV::write_images(const char* file_path) const
{
    const bf::path boost_file_path(file_path);
    const bf::path directory = boost_file_path.parent_path();
    const string base_file_name = boost_file_path.stem().string();
    const string extension = boost_file_path.extension().string();

    const string exr_file_name = base_file_name + ".cryptomatte" + extension;
    const string exr_file_path = (directory / exr_file_name).string();

    string layer_name = "AppleseedObjectName";

    switch (impl->m_layer_type)
    {
      case CryptomatteType::ObjectNames:
        layer_name = "AppleseedObjectName";
        break;

      case CryptomatteType::MaterialNames:
        layer_name = "AppleseedMaterialName";
        break;

      default:
        break;
    }

    uint32_t type_name_hash = 0;
    MurmurHash3_x86_32(reinterpret_cast<const unsigned char*>(layer_name.c_str()), static_cast<int>(layer_name.size()), 0, &type_name_hash);

    char type_name_hex[9];
    sprintf(type_name_hex, "%08x", type_name_hash);

    ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
    image_attributes.insert("color_space", "linear");

    string layer_prefix("cryptomatte/");
    layer_prefix.append(string(type_name_hex).erase(7));
    image_attributes.insert(layer_prefix + "/conversion", "uint32_to_float32");
    image_attributes.insert(layer_prefix + "/hash", "MurmurHash3_32");
    image_attributes.insert(layer_prefix + "/name", layer_name);

    NameMap name_map;
    for (auto& pair : impl->m_tile_name_map)
    {
        for (auto& hash : pair.second)
        {
            if (hash.first == 0)
                continue;
            name_map[hash.first] = hash.second;
        }
    }

    string manifest_str;
    for (auto& hash : name_map)
    {
        char hash_hex[9];
        sprintf(hash_hex, "%08x", hash.first);
        manifest_str.append(format("\"{0}\":\"{1}\",", hash.second, string(hash_hex)));
    }
    if (manifest_str.size() > 0)
        manifest_str.pop_back();

    const string manifest_string = format("{{0}}", manifest_str);

    image_attributes.insert(layer_prefix + "/manifest", manifest_string.c_str());

    vector<string> channel_names;
    channel_names.push_back("R");
    channel_names.push_back("G");
    channel_names.push_back("B");

    int num_exr_layers = truncate<int>(fast_ceil(impl->m_num_layers / 2.0));
    for(size_t i = 0; i < num_exr_layers; ++i)
    {
        string layer_number = get_numbered_string("##", i);
        channel_names.push_back(format("{0}{1}.R", layer_name, layer_number));
        channel_names.push_back(format("{0}{1}.G", layer_name, layer_number));
        channel_names.push_back(format("{0}{1}.B", layer_name, layer_number));
        channel_names.push_back(format("{0}{1}.A", layer_name, layer_number));
    }

    const Image& image = *impl->m_image;

    try
    {
        const int num_channels = (impl->m_num_layers * 2) + 3;
        MultiChannelExrFileWriter writer(exr_file_path.c_str(), &image);
        writer.set_image_channels(num_channels, channel_names);
        writer.set_image_attributes(image_attributes);
        writer.write();
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to write cryptomatte file %s: %s.",
            exr_file_path.c_str(),
            e.what());

        return false;
    }

    return true;
}


//
// CryptomatteAOVFactory class implementation.
//

auto_release_ptr<CryptomatteAOV> CryptomatteAOVFactory::create(
    const ParamArray&           params)
{
    const string cryptomatte_type = params.get_optional<string>("cryptomatte_type", "off");
    CryptomatteAOV::CryptomatteType layer_type;
    if (cryptomatte_type == "object_names")
        layer_type = CryptomatteAOV::CryptomatteType::ObjectNames;
    else if (cryptomatte_type == "material_names")
        layer_type = CryptomatteAOV::CryptomatteType::MaterialNames;

    return auto_release_ptr<CryptomatteAOV>(new CryptomatteAOV(
        params.get_optional<int>("cryptomatte_num_layers", 6),
        layer_type));
}

}   // namespace renderer
