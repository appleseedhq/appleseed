
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "attributeset.h"

// Standard headers.
#include <cstring>

namespace foundation
{

//
// AttributeSet class implementation.
//

AttributeSet::~AttributeSet()
{
    for (size_t i = 0; i < m_channels.size(); ++i)
        delete m_channels[i];
}

AttributeSet::ChannelID AttributeSet::create_channel(
    const std::string&       name,
    const NumericTypeID      type,
    const size_t             dimension)
{
    assert(dimension > 0);

    Channel* channel = new Channel();
    channel->m_name = name;
    channel->m_type = type;
    channel->m_dimension = dimension;
    channel->m_value_size = NumericType::size(type) * dimension;
    m_channels.push_back(channel);

    return m_channels.size() - 1;
}

void AttributeSet::delete_channel(const ChannelID channel_id)
{
    assert(channel_id < m_channels.size());

    delete m_channels[channel_id];

    m_channels.erase(m_channels.begin() + channel_id);
}

AttributeSet::ChannelID AttributeSet::find_channel(const char* name) const
{
    assert(name);

    const size_t channel_count = m_channels.size();

    for (size_t i = 0; i < channel_count; ++i)
    {
        if (!strcmp(m_channels[i]->m_name.c_str(), name))
            return i;
    }

    return InvalidChannelID;
}

}   // namespace foundation
