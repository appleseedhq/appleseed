
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
#include "cryptomatteaov.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/stopwatch.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Murmurhash3 headers.
#include "MurmurHash3.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "OpenImageIO/version.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace foundation;
namespace bf = boost::filesystem;

namespace
{
    //
    // Reference:
    //
    //   Cryptomatte Specification
    //   https://github.com/Psyop/Cryptomatte/blob/master/specification/cryptomatte_specification.pdf
    //

    class MultiChannelExrFileWriter
      : public IImageFileWriter
    {
      public:
        MultiChannelExrFileWriter(const char* filename, const ICanvas* image)
          : m_canvas(image)
          , m_filename(filename)
        {
            m_writer = OIIO::ImageOutput::create(m_filename);

            if (m_writer == nullptr)
            {
                const std::string msg = OIIO::geterror();
                throw ExceptionIOError(msg.c_str());
            }

            m_spec = OIIO::ImageSpec();
            set_image_spec();
        }

        ~MultiChannelExrFileWriter() override
        {
#if OIIO_VERSION < 20000
            // Destroy the ImageOutput stucture.
            if (m_writer != nullptr)
                OIIO::ImageOutput::destroy(m_writer);
#endif
        }

        void write()
        {
            if (!m_writer->supports("tiles"))
                return;

            if (!m_writer->open(m_filename, m_spec))
            {
                const std::string msg = m_writer->geterror();
                throw ExceptionIOError(msg.c_str());
            }

            write_tiles();
            close_file();
        }

        void set_image_attributes(const ImageAttributes& image_attributes)
        {
            for (const auto& attribute : image_attributes)
            {
                // Fetch the name and the value of the attribute.
                const std::string attr_name = attribute.key();
                const std::string attr_value = attribute.value<std::string>();

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
            const size_t                      channel_count,
            const std::vector<std::string>&   channel_names)
        {
            m_spec.nchannels = static_cast<int>(channel_count);

            for (size_t i = 0; i < channel_count; i++)
                m_spec.channelnames.push_back(channel_names[i]);
        }

      private:
        const ICanvas*                      m_canvas;
        OIIO::ImageSpec                     m_spec;
#if OIIO_VERSION >= 20000
        std::unique_ptr<OIIO::ImageOutput>  m_writer;
#else
        OIIO::ImageOutput*                  m_writer;
#endif
        const char*                         m_filename;

        void close_file()
        {
            // Close the image file.
            if (!m_writer->close())
            {
                const std::string msg = m_writer->geterror();
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
                    const size_t ystride = xstride * std::min(static_cast<size_t>(m_spec.width + m_spec.x - tile_offset_x),
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
                        const std::string msg = m_writer->geterror();
                        close_file();
                        throw ExceptionIOError(msg.c_str());
                    }
                }
            }
        }
    };

    class WeightMap
    {
      public:
        struct Entry
        {
            std::uint32_t   m_key;
            float           m_value;

            Entry()
              : m_key(0)
              , m_value(0.0f)
            {
            }
        };

        explicit WeightMap(const size_t size)
          : m_size(static_cast<std::uint32_t>(size))
          , m_index(0)
        {
            assert(size > 0);
            m_map = new Entry[size];
        }

        WeightMap(const WeightMap& other)
          : m_size(other.m_size)
          , m_index(other.m_index)
        {
            m_map = new Entry[m_size];
            for (size_t i = 0; i < m_index; ++i)
                m_map[i] = other.m_map[i];
        }

        WeightMap& operator=(const WeightMap& rhs)
        {
            if (m_size != rhs.m_size)
            {
                delete[] m_map;
                m_map = new Entry[rhs.m_size];
            }

            m_size = rhs.m_size;
            m_index = rhs.m_size;
            for (size_t i = 0; i < m_index; ++i)
                m_map[i] = rhs.m_map[i];

            return *this;
        }

        ~WeightMap()
        {
            delete[] m_map;
        }

        void insert(const std::uint32_t key, const float value)
        {
            assert(m_map);

            for (size_t i = 0; i < m_index; ++i)
            {
                if (m_map[i].m_key == key)
                {
                    m_map[i].m_value = value;
                    return;
                }
            }

            if (m_index < m_size)
            {
                m_map[m_index].m_key = key;
                m_map[m_index].m_value = value;
                ++m_index;
            }
        }

        float get(const std::uint32_t key) const
        {
            assert(m_map);

            for (size_t i = 0; i < m_index; ++i)
            {
                if (m_map[i].m_key == key)
                    return m_map[i].m_value;
            }

            return 0.0f;
        }

        void clear()
        {
            m_index = 0;
        }

        bool empty() const
        {
            return m_index == 0;
        }

        Entry* begin() const
        {
            return m_map;
        }

        Entry* end() const
        {
            return m_map + m_index;
        }

      private:
        std::uint32_t   m_size;
        std::uint32_t   m_index;
        Entry*          m_map;
    };

    // Code taken from Cryptomatte specification.
    float hash_to_float(std::uint32_t hash)
    {
        // if all exponent bits are 0 (subnormals, +zero, -zero) set exponent to 1
        // if all exponent bits are 1 (NaNs, +inf, -inf) set exponent to 254
        const std::uint32_t exponent = hash >> 23 & 255; // extract exponent (8 bits)
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
    typedef std::map<std::uint32_t, std::string> NameMap;


    //
    // Cryptomatte AOV accumulator.
    //

    class CryptomatteAOVAccumulator
      : public AOVAccumulator
    {
      public:
        CryptomatteAOVAccumulator(
            Image&                              aov_image,
            std::vector<WeightMap>&             pixel_samples,
            NameMap*                            tile_name_array,
            size_t                              num_layers,
            CryptomatteAOV::CryptomatteType     layer_type)
          : m_aov_image(aov_image)
          , m_num_layers(num_layers)
          , m_pixel_samples(pixel_samples)
          , m_tile_name_maps(tile_name_array)
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
            m_tile_index = tile_y * props.m_tile_count_y + tile_x;

            // Fetch the tile bounds (inclusive).
            m_tile_origin_x = tile_x * props.m_tile_width;
            m_tile_origin_y = tile_y * props.m_tile_height;
            m_tile_end_x = m_tile_origin_x + tile.get_width() - 1;
            m_tile_end_y = m_tile_origin_y + tile.get_height() - 1;

            for (size_t ry = m_tile_origin_y; ry <= m_tile_end_y; ++ry)
            {
                for (size_t rx = m_tile_origin_x; rx <= m_tile_end_x; ++rx)
                    m_pixel_samples[ry * m_frame_width + rx].clear();
            }

            m_crop_window =
                frame.has_crop_window()
                    ? frame.get_crop_window()
                    : AABB2u(Vector2u(m_tile_origin_x, m_tile_origin_y), Vector2u(m_tile_end_x, m_tile_end_y));
        }

        void on_tile_end(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y) override
        {
            std::vector<std::pair<float, std::uint32_t>> ranked_vector;
            std::vector<float> pixel_values;
            constexpr float uint32_max_rcp = 1.0f / std::numeric_limits<std::uint32_t>::max();

            for (size_t ry = m_tile_origin_y; ry <= m_tile_end_y; ++ry)
            {
                for (size_t rx = m_tile_origin_x; rx <= m_tile_end_x; ++rx)
                {
                    const WeightMap& weight_map = m_pixel_samples[ry * m_frame_width + rx];

                    if (!weight_map.empty())
                    {
                        clear_keep_memory(ranked_vector);
                        clear_keep_memory(pixel_values);

                        float total_weight = 0.0f;
                        for (const auto& item : weight_map)
                            total_weight += item.m_value;

                        if (total_weight == 0.0f)
                            total_weight = 1.0f;

                        for (const auto& item : weight_map)
                            ranked_vector.push_back(std::make_pair(item.m_value, item.m_key));

                        sort(ranked_vector.begin(), ranked_vector.end(),
                            [](std::pair<float, std::uint32_t> const& a, std::pair<float, std::uint32_t> const& b)
                            {
                                return a.first > b.first;
                            });

                        const std::uint32_t m3hash_preview = ranked_vector[0].second;

                        // Preview channels (deprecated in recent Cryptomatte specification).
                        float r(0.0f), g(0.0f), b(0.0f);
                        if (m3hash_preview != 0)
                        {
                            r = hash_to_float(m3hash_preview);
                            g = static_cast<float>(m3hash_preview << 8) * uint32_max_rcp;
                            b = static_cast<float>(m3hash_preview << 16) * uint32_max_rcp;
                        }
                        pixel_values.push_back(r);
                        pixel_values.push_back(g);
                        pixel_values.push_back(b);

                        // Remove background contribution.
                        size_t ranked_vector_start = 0;
                        if (ranked_vector.size() > 1 && m3hash_preview == 0)
                            ranked_vector_start = 1;

                        // Ranked channels.
                        for (size_t i = ranked_vector_start, e = std::min(ranked_vector.size(), m_num_layers); i < e; ++i)
                        {
                            const std::uint32_t m3hash = ranked_vector[i].second;
                            float rank(0.0f), coverage(0.0f);
                            if (m3hash != 0)
                            {
                                rank = hash_to_float(m3hash);
                                coverage = ranked_vector[i].first / total_weight;
                            }
                            pixel_values.push_back(rank);
                            pixel_values.push_back(coverage);
                        }

                        // Set the remaining channels of the pixel to black.
                        // This is determined by taking 2 channels per ranked vector plus
                        // 3 channels for the preview image and subtracting that from the
                        // total number of AOV channels.
                        const size_t num_channels = (m_num_layers * 2) + 3;
                        const size_t filled_channels = (ranked_vector.size() - ranked_vector_start) * 2 + 3;

                        for (size_t i = filled_channels; i < num_channels; ++i)
                            pixel_values.push_back(0.0f);

                        m_aov_image.set_pixel(rx, ry, pixel_values.data(), pixel_values.size());
                    }
                }
            }
        }

        void on_sample_begin(const PixelContext& pixel_context) override
        {
        }

        void on_sample_end(const PixelContext& pixel_context) override
        {
        }

        void on_pixel_begin(const Vector2i& pi) override
        {
        }

        void on_pixel_end(const Vector2i& pi) override
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            std::uint32_t m3hash = 0;
            std::string obj_name;

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

                  assert_otherwise;
                }

                MurmurHash3_x86_32(reinterpret_cast<const unsigned char*>(obj_name.c_str()), static_cast<int>(obj_name.size()), 0, &m3hash);
            }

            const Vector2u pixel_pos(pixel_context.get_pixel_coords());

            // Ignore samples outside the crop window.
            if (!m_crop_window.contains(pixel_pos))
                return;

            m_tile_name_maps[m_tile_index][m3hash] = obj_name;

            const size_t x = pixel_pos.x - m_tile_origin_x;
            const size_t y = pixel_pos.y - m_tile_origin_y;

            WeightMap& weight_map =
                m_pixel_samples[(m_tile_origin_y + y) * m_frame_width + (m_tile_origin_x + x)];

            const float old_value = weight_map.get(m3hash);
            weight_map.insert(m3hash, old_value + 1.0f);
        }

      private:
        size_t                          m_tile_origin_x;
        size_t                          m_tile_origin_y;
        size_t                          m_tile_end_x;
        size_t                          m_tile_end_y;
        size_t                          m_tile_index;
        size_t                          m_frame_width;
        AABB2u                          m_crop_window;
        Image&                          m_aov_image;
        size_t                          m_num_layers;
        std::vector<WeightMap>&         m_pixel_samples;
        NameMap*                        m_tile_name_maps;
        CryptomatteAOV::CryptomatteType m_layer_type;
    };
}


//
// CryptomatteAOV class implementation.
//

namespace
{
    const char* CryptomatteObjectAOVModel = "cryptomatte_object_aov";
    const char* CryptomatteMaterialAOVModel = "cryptomatte_material_aov";
}

struct CryptomatteAOV::Impl
{
    std::vector<WeightMap>              m_pixel_samples;
    NameMap*                            m_tile_name_maps;
    std::unique_ptr<Image>              m_image;
    size_t                              m_num_layers;
    CryptomatteAOV::CryptomatteType     m_layer_type;

    static std::string make_manifest(const NameMap& name_map)
    {
        std::stringstream sstr;
        sstr << "{";

        for (const auto& entry : name_map)
        {
            char hash_hex[9];
            sprintf(hash_hex, "%08x", entry.first);
            sstr << "\"" << entry.second << "\":\"" << hash_hex << "\",";
        }

        // Remove trailing comma.
        if (!name_map.empty())
            sstr.seekp(-1, sstr.end);

        sstr << "}";
        return sstr.str();
    }

    NameMap make_name_map() const
    {
        NameMap name_map;

        for (size_t i = 0, e = m_image->properties().m_tile_count; i < e; ++i)
        {
            for (const auto& entry : m_tile_name_maps[i])
            {
                if (entry.first != 0)
                    name_map[entry.first] = entry.second;
            }
        }

        return name_map;
    }

    std::vector<std::string> make_channel_names(const std::string& layer_name) const
    {
        std::vector<std::string> channel_names;
        channel_names.reserve(get_channel_count());

        channel_names.push_back("R");
        channel_names.push_back("G");
        channel_names.push_back("B");

        const size_t exr_layer_count = truncate<size_t>(std::ceil(m_num_layers / 2.0));
        for (size_t i = 0; i < exr_layer_count; ++i)
        {
            const std::string layer_number = get_numbered_string("##", i);
            channel_names.push_back(format("{0}{1}.R", layer_name, layer_number));
            channel_names.push_back(format("{0}{1}.G", layer_name, layer_number));
            channel_names.push_back(format("{0}{1}.B", layer_name, layer_number));
            channel_names.push_back(format("{0}{1}.A", layer_name, layer_number));
        }

        return channel_names;
    }

    Impl()
      : m_tile_name_maps(nullptr)
    {
    }

    size_t get_channel_count() const
    {
        // Each object layer ("rank" in Cryptomatte specification) requires two image channels: ID and Coverage.
        // The total number of image channels is equal to three preview channels (R, G, B) plus ID and Coverage
        // channels for each object layer.
        return 3 + (m_num_layers * 2);
    }
};

CryptomatteAOV::CryptomatteAOV(const ParamArray& params)
  : AOV("cryptomatte", params)
  , impl(new Impl())
{
    const std::string cryptomatte_type =
        params.get_optional<std::string>(
            "cryptomatte_type",
            "object_names",
            make_vector("object_names" , "material_names"));

    if (cryptomatte_type == "object_names")
    {
        impl->m_layer_type = CryptomatteType::ObjectNames;
        set_name(CryptomatteObjectAOVModel);
    }
    else if (cryptomatte_type == "material_names")
    {
        impl->m_layer_type = CryptomatteType::MaterialNames;
        set_name(CryptomatteMaterialAOVModel);
    }

    impl->m_num_layers = params.get_optional<size_t>("cryptomatte_num_layers", 6);
}

CryptomatteAOV::~CryptomatteAOV()
{
    delete[] impl->m_tile_name_maps;
    delete impl;
}

const char* CryptomatteAOV::get_model() const
{
    return
        impl->m_layer_type == CryptomatteAOV::CryptomatteType::ObjectNames
            ? CryptomatteObjectAOVModel
            : CryptomatteMaterialAOVModel;
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

Image* CryptomatteAOV::get_cryptomatte_image() const
{
    return impl->m_image.get();
}

void CryptomatteAOV::create_image(
    const size_t    canvas_width,
    const size_t    canvas_height,
    const size_t    tile_width,
    const size_t    tile_height,
    ImageStack&     aov_images)
{
    // Create underlying images.
    const size_t channel_count = impl->get_channel_count();
    impl->m_image.reset(
        new Image(
            canvas_width,
            canvas_height,
            tile_width,
            tile_height,
            channel_count,
            PixelFormatFloat));
    impl->m_pixel_samples.resize(canvas_width * canvas_height, WeightMap(impl->m_num_layers));
    const auto& image_props = impl->m_image->properties();
    impl->m_tile_name_maps = new NameMap[image_props.m_tile_count];
    clear_image();
}

void CryptomatteAOV::clear_image()
{
    const auto& image_props = impl->m_image->properties();

    const size_t channel_count = impl->get_channel_count();
    const std::vector<float> pixel_values(channel_count, 0.0f);

    for (size_t y = 0, h = image_props.m_canvas_height; y < h; ++y)
    {
        for (size_t x = 0, w = image_props.m_canvas_width; x < w; ++x)
            impl->m_image->set_pixel(x, y, pixel_values.data(), pixel_values.size());
    }

    for (size_t i = 0, e = image_props.m_tile_count; i < e; ++i)
        impl->m_tile_name_maps[i].clear();
}

auto_release_ptr<AOVAccumulator> CryptomatteAOV::create_accumulator() const
{
    return
        auto_release_ptr<AOVAccumulator>(
            new CryptomatteAOVAccumulator(
                *impl->m_image,
                impl->m_pixel_samples,
                impl->m_tile_name_maps,
                impl->m_num_layers,
                impl->m_layer_type));
}

bool CryptomatteAOV::write_images(
    const char*             file_path,
    const ImageAttributes&  image_attributes) const
{
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    const bf::path boost_file_path(file_path);
    const bf::path directory = boost_file_path.parent_path();
    const std::string base_file_name = boost_file_path.stem().string();
    const std::string extension = boost_file_path.extension().string();

    const std::string exr_file_name = base_file_name + ".cryptomatte" + extension;
    const std::string exr_file_path = (directory / exr_file_name).string();

    std::string layer_name;
    switch (impl->m_layer_type)
    {
      case CryptomatteType::ObjectNames:
        layer_name = "AppleseedObjectName";
        break;

      case CryptomatteType::MaterialNames:
        layer_name = "AppleseedMaterialName";
        break;

      assert_otherwise;
    }

    std::uint32_t type_name_hash = 0;
    MurmurHash3_x86_32(reinterpret_cast<const unsigned char*>(layer_name.c_str()), static_cast<int>(layer_name.size()), 0, &type_name_hash);

    char type_name_hex[9];
    std::sprintf(type_name_hex, "%08x", type_name_hash);
    const std::string layer_prefix = format("cryptomatte/{0}", std::string(type_name_hex).substr(0, 7));

    const NameMap name_map = impl->make_name_map();
    const std::string manifest = Impl::make_manifest(name_map);

    ImageAttributes image_attributes_copy(image_attributes);
    image_attributes_copy.insert("color_space", "linear");
    image_attributes_copy.insert(std::string(layer_prefix + "/name").c_str(), layer_name);
    image_attributes_copy.insert(std::string(layer_prefix + "/hash").c_str(), "MurmurHash3_32");
    image_attributes_copy.insert(std::string(layer_prefix + "/conversion").c_str(), "uint32_to_float32");
    image_attributes_copy.insert(std::string(layer_prefix + "/manifest").c_str(), manifest);

    const size_t channel_count = impl->get_channel_count();
    const std::vector<std::string> channel_names = impl->make_channel_names(layer_name);

    try
    {
        MultiChannelExrFileWriter writer(exr_file_path.c_str(), impl->m_image.get());
        writer.set_image_channels(channel_count, channel_names);
        writer.set_image_attributes(image_attributes_copy);
        writer.write();
    }
    catch (const std::exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to write cryptomatte file %s for aov \"%s\": %s.",
            exr_file_path.c_str(),
            get_path().c_str(),
            e.what());

        return false;
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
        "wrote image file %s for aov \"%s\" in %s.",
        file_path,
        get_path().c_str(),
        pretty_time(stopwatch.get_seconds()).c_str());

    return true;
}


//
// CryptomatteAOVFactory class implementation.
//

CryptomatteAOVFactory::CryptomatteAOVFactory(const CryptomatteAOV::CryptomatteType aov_type)
  : m_aov_type(aov_type)
{
}

void CryptomatteAOVFactory::release()
{
    delete this;
}

const char* CryptomatteAOVFactory::get_model() const
{
    return
        m_aov_type == CryptomatteAOV::CryptomatteType::ObjectNames
            ? CryptomatteObjectAOVModel
            : CryptomatteMaterialAOVModel;
}

Dictionary CryptomatteAOVFactory::get_model_metadata() const
{
    Dictionary metadata;
    metadata.insert("name", get_model());

    metadata.insert("label",
        m_aov_type == CryptomatteAOV::CryptomatteType::ObjectNames
            ? "CryptomatteObjects"
            : "CryptomatteMaterials");

    return metadata;
}

DictionaryArray CryptomatteAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> CryptomatteAOVFactory::create(const ParamArray& params) const
{
    ParamArray new_params(params);

    new_params.insert("cryptomatte_type",
        m_aov_type == CryptomatteAOV::CryptomatteType::ObjectNames
            ? "object_names"
            : "material_names");

    return auto_release_ptr<AOV>(new CryptomatteAOV(new_params));
}

}   // namespace renderer
