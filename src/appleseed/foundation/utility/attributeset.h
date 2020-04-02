
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/memory.h"
#include "foundation/utility/numerictype.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace foundation
{

//
// A set of named and typed attributes.
//

class AttributeSet
  : public NonCopyable
{
  public:
    typedef size_t ChannelID;
    static const ChannelID InvalidChannelID = ~ChannelID(0);

    // Destructor.
    ~AttributeSet();

    // Create a new attribute channel.
    ChannelID create_channel(
        const std::string&  name,
        const NumericTypeID type,
        const size_t        dimension);

    // Delete an existing channel.
    void delete_channel(const ChannelID channel_id);

    // Find a given attribute channel. Return InvalidChannelID if
    // the requested channel does not exist. Since this method is
    // typically called with a literal value in argument ("uv"),
    // it takes a C string instead of std::string, thus avoiding
    // the cost of constructing a std::string object.
    ChannelID find_channel(const char* name) const;

    // Return the number of attributes in a given attribute channel.
    size_t get_attribute_count(const ChannelID channel_id) const;

    // Reserve memory for a given number of atributes in a given attribute channel.
    void reserve_attributes(
        const ChannelID     channel_id,
        const size_t        count);

    // Insert a new attribute at the end of a given attribute channel.
    // Return the index of the attribute in the attribute channel.
    template <typename T>
    size_t push_attribute(
        const ChannelID     channel_id,
        const T&            value);

    // Set a given attribute.
    template <typename T>
    void set_attribute(
        const ChannelID     channel_id,
        const size_t        index,
        const T&            value);

    // Get a given attribute.
    template <typename T>
    void get_attribute(
        const ChannelID     channel_id,
        const size_t        index,
        T*                  value) const;

  private:
    struct Channel
    {
        std::string                 m_name;
        NumericTypeID               m_type;
        size_t                      m_dimension;
        size_t                      m_value_size;
        std::vector<std::uint8_t>   m_storage;
    };

    std::vector<Channel*>   m_channels;
};


//
// AttributeSet class implementation.
//

inline size_t AttributeSet::get_attribute_count(const ChannelID channel_id) const
{
    // Get the channel descriptor.
    assert(channel_id < m_channels.size());
    Channel* channel = m_channels[channel_id];

    return channel->m_storage.size() / channel->m_value_size;
}

inline void AttributeSet::reserve_attributes(
    const ChannelID         channel_id,
    const size_t            count)
{
    // Get the channel descriptor.
    assert(channel_id < m_channels.size());
    Channel* channel = m_channels[channel_id];

    // Reserve memory.
    channel->m_storage.reserve(count * channel->m_value_size);
}

template <typename T>
inline size_t AttributeSet::push_attribute(
    const ChannelID         channel_id,
    const T&                value)
{
    // Get the channel descriptor.
    assert(channel_id < m_channels.size());
    Channel* channel = m_channels[channel_id];

    // Check that the size of the attribute matches the size in the channel descriptor.
    assert(channel->m_value_size == sizeof(T));

    const size_t current_size = channel->m_storage.size();
    const size_t index = current_size / sizeof(T);

    // Resize the storage to accomodate the new attribute.
    channel->m_storage.resize(current_size + sizeof(T));

    // Store the new attribute.
    T* typed_storage = reinterpret_cast<T*>(&channel->m_storage.front());
    typed_storage[index] = value;

    // Return the index of the new attribute.
    return index;
}

template <typename T>
inline void AttributeSet::set_attribute(
    const ChannelID         channel_id,
    const size_t            index,
    const T&                value)
{
    // Get the channel descriptor.
    assert(channel_id < m_channels.size());
    Channel* channel = m_channels[channel_id];

    // Check that the size of the attribute matches the size in the channel descriptor.
    assert(channel->m_value_size == sizeof(T));

    // Resize the storage to accommodate the new attribute.
    const size_t new_size = (index + 1) * sizeof(T);
    ensure_minimum_size(channel->m_storage, new_size);

    // Store the new attribute.
    T* typed_storage = reinterpret_cast<T*>(&channel->m_storage.front());
    typed_storage[index] = value;
}

template <typename T>
inline void AttributeSet::get_attribute(
    const ChannelID         channel_id,
    const size_t            index,
    T*                      value) const
{
    // Get the channel descriptor.
    assert(channel_id < m_channels.size());
    const Channel* channel = m_channels[channel_id];

    // Check that the size of the attribute matches the size in the channel descriptor.
    assert(channel->m_value_size == sizeof(T));

    assert(index * sizeof(T) < channel->m_storage.size());
    assert(value);

    // Read the attribute.
    const T* typed_storage = reinterpret_cast<const T*>(&channel->m_storage.front());
    *value = typed_storage[index];
}

}   // namespace foundation
