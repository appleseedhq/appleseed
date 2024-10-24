// This file comes from the original BCD implementation,
// with minor changes to remove dependencies, unused code
// and re-formatting. Original license follows:

// This file is part of the reference implementation for the paper
//   Bayesian Collaborative Denoising for Monte-Carlo Rendering
//   Malik Boughida and Tamy Boubekeur.
//   Computer Graphics Forum (Proc. EGSR 2017), vol. 36, no. 4, p. 137-153, 2017
//
// All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.txt file.

// BCD headers.
#include "DeepImage.h"
#include "ImageIO.h"

// OpenEXR headers.
#if defined _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4244)     // conversion from 'std::streamsize' to 'int', possible loss of data
    #pragma warning (disable: 4800)     // 'int': forcing value to bool 'true' or 'false' (performance warning)
#endif

#if __GNUC__ >= 7
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wdeprecated"
#endif

    #include <ImfChannelList.h>
    #include <ImfFrameBuffer.h>
    #include <ImfInputFile.h>
    #include <ImfOutputFile.h>

#if __GNUC__ >= 7
    #pragma GCC diagnostic pop
#endif

#if defined _MSC_VER
    #pragma warning (pop)
#endif

// Standard headers.
#include <stdio.h>
#include <stdlib.h>

using namespace Imath;
using namespace Imf;
using namespace std;

namespace bcd
{

typedef DeepImage<float> Deepimf;

bool ImageIO::loadEXR(Deepimf& o_rImage, const char* i_pFilePath)
{
    Imf::InputFile file(i_pFilePath);

    const Imath::Box2i& data_window = file.header().dataWindow();
    const int width = data_window.size().x + 1;
    const int height = data_window.size().y + 1;

    const int depth = 3;
    o_rImage.resize(width, height, depth);

    const int float_size = static_cast<int>(sizeof(float));
    const int x_stride = o_rImage.glueIndices(0, 1, 0) * float_size;
    const int y_stride = o_rImage.glueIndices(1, 0, 0) * float_size;
    const int c_stride = o_rImage.glueIndices(0, 0, 1) * float_size;

    char* p = reinterpret_cast<char*>(o_rImage.getDataPtr());
    p -= data_window.min.x * x_stride;
    p -= data_window.min.y * y_stride;

    FrameBuffer framebuffer;

    const char* channel_names[3] = {"R", "G", "B"};

    for (size_t i = 0; i < depth; ++i)
    {
        framebuffer.insert(
            channel_names[i],
            Imf::Slice(
                Imf::FLOAT,
                p,
                static_cast<size_t>(x_stride),
                static_cast<size_t>(y_stride),
                1,
                1,
                0.0));
        p += c_stride;
    }

    file.setFrameBuffer(framebuffer);
    file.readPixels(data_window.min.y, data_window.max.y);

    return true;
}

bool ImageIO::writeEXR(const Deepimf& i_rImage, const char* i_pFilePath)
{
    if (i_rImage.getDepth() != 3)
        return false;

    const int float_size = static_cast<int>(sizeof(float));
    const int x_stride = i_rImage.glueIndices(0, 1, 0) * float_size;
    const int y_stride = i_rImage.glueIndices(1, 0, 0) * float_size;
    const int c_stride = i_rImage.glueIndices(0, 0, 1) * float_size;

    char* p = const_cast<char*>(reinterpret_cast<const char*>(i_rImage.getDataPtr()));

    FrameBuffer framebuffer;
    const char* channel_names[3] = {"R", "G", "B"};

    Header header(i_rImage.getWidth(), i_rImage.getHeight());

    for (int i = 0; i < i_rImage.getDepth(); ++i)
    {
        header.channels().insert(channel_names[i], Channel(FLOAT));

        framebuffer.insert(
            channel_names[i],
            Imf::Slice(
                Imf::FLOAT,
                p,
                static_cast<size_t>(x_stride),
                static_cast<size_t>(y_stride)));
        p += c_stride;
    }

    OutputFile file(i_pFilePath, header);
    file.setFrameBuffer(framebuffer);
    file.writePixels(i_rImage.getHeight());

    return true;
}

namespace
{
    void bin_channel_name(char* channel_name, const size_t n, const int i)
    {
#ifdef _WIN32
        _snprintf(channel_name, n, "Bin_%04d", i);
#else
        snprintf(channel_name, n, "Bin_%04d", i);
#endif
    }
}

bool ImageIO::loadMultiChannelsEXR(Deepimf& o_rImage, const char* i_pFilePath)
{
    InputFile file(i_pFilePath);

    Box2i data_window = file.header().dataWindow();

    int width = data_window.size().x + 1;
    int height = data_window.size().y + 1;

    // Count the number of histogram channels.
    const ChannelList& channel_list = file.header().channels();

    int num_bins = 0;

    while(true)
    {
        char channel_name[100];
        bin_channel_name(channel_name, sizeof(channel_name), num_bins);

        if (channel_list.findChannel(channel_name) == nullptr)
            break;

        ++num_bins;
    }

    o_rImage.resize(width, height, num_bins);

    const int float_size = static_cast<int>(sizeof(float));
    const int x_stride = o_rImage.glueIndices(0, 1, 0) * float_size;
    const int y_stride = o_rImage.glueIndices(1, 0, 0) * float_size;
    const int c_stride = o_rImage.glueIndices(0, 0, 1) * float_size;

    char* p = reinterpret_cast<char*>(o_rImage.getDataPtr());
    p -= data_window.min.x * x_stride;
    p -= data_window.min.y * y_stride;

    FrameBuffer framebuffer;

    for (int i = 0; i < num_bins; ++i)
    {
        char channel_name[100];
        bin_channel_name(channel_name, sizeof(channel_name), i);

        framebuffer.insert(
            channel_name,
            Slice(
                FLOAT,
                p,
                static_cast<size_t>(x_stride),
                static_cast<size_t>(y_stride)));
        p += c_stride;
    }

    file.setFrameBuffer(framebuffer);

    file.readPixels(data_window.min.y, data_window.max.y);

    return true;
}

bool ImageIO::writeMultiChannelsEXR(const Deepimf& i_rImage, const char* i_pFilePath)
{
    char* p = const_cast<char*>(reinterpret_cast<const char*>(i_rImage.getDataPtr()));

    const int float_size = static_cast<int>(sizeof(float));
    const int x_stride = i_rImage.glueIndices(0, 1, 0) * float_size;
    const int y_stride = i_rImage.glueIndices(1, 0, 0) * float_size;
    const int c_stride = i_rImage.glueIndices(0, 0, 1) * float_size;

    Header header(i_rImage.getWidth(), i_rImage.getHeight());
    FrameBuffer framebuffer;

    for (int i = 0; i < i_rImage.getDepth(); i++)
    {
        char channel_name[100];
        bin_channel_name(channel_name, sizeof(channel_name), i);

        header.channels().insert(channel_name, Channel(FLOAT));

        framebuffer.insert(
            channel_name,
            Imf::Slice(
                Imf::FLOAT,
                p,
                static_cast<size_t>(x_stride),
                static_cast<size_t>(y_stride)));
        p += c_stride;
    }

    OutputFile file(i_pFilePath, header);
    file.setFrameBuffer(framebuffer);
    file.writePixels(i_rImage.getHeight());

    return true;
}

} // namespace bcd
