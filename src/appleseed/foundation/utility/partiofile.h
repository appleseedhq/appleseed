
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_PARTIOFILE_H
#define APPLESEED_FOUNDATION_UTILITY_PARTIOFILE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/math/vector.h"

// Partio headers.
#include <Partio.h>

namespace foundation
{

class PartioFile
  : public NonCopyable
{
  public:
    PartioFile();
    ~PartioFile();

    Partio::ParticleAttribute add_float_attribute(const char* name);
    Partio::ParticleAttribute add_vector_attribute(const char* name);
    Partio::ParticleAttribute add_color_attribute(const char* name);

    Partio::ParticleIndex add_particle();

    template <typename T>
    void set_float_attribute(
        const Partio::ParticleIndex         particle,
        const Partio::ParticleAttribute&    attribute,
        const T                             value);

    template <typename T>
    void set_vector_attribute(
        const Partio::ParticleIndex         particle,
        const Partio::ParticleAttribute&    attribute,
        const foundation::Vector<T,3>&      value);

    template <typename T>
    void set_color_attribute(
        const Partio::ParticleIndex         particle,
        const Partio::ParticleAttribute&    attribute,
        const foundation::Color<T,3>&       value);

    void write(const char* filepath) const;

  private:
    Partio::ParticlesDataMutable*   m_particles;
};


//
// PartioFile class implementation.
//

template <typename T>
inline void PartioFile::set_float_attribute(
    const Partio::ParticleIndex         particle,
    const Partio::ParticleAttribute&    attribute,
    const T                             value)
{
    float* data = m_particles->dataWrite<float>(attribute, particle);
    *data = static_cast<float>(value);
}

template <typename T>
inline void PartioFile::set_vector_attribute(
    const Partio::ParticleIndex         particle,
    const Partio::ParticleAttribute&    attribute,
    const foundation::Vector<T,3>&      value)
{
    float* data = m_particles->dataWrite<float>(attribute, particle);
    data[0] = static_cast<float>(value[0]);
    data[1] = static_cast<float>(value[1]);
    data[2] = static_cast<float>(value[2]);
}

template <typename T>
inline void PartioFile::set_color_attribute(
    const Partio::ParticleIndex         particle,
    const Partio::ParticleAttribute&    attribute,
    const foundation::Color<T,3>&       value)
{
    float* data = m_particles->dataWrite<float>(attribute, particle);
    data[0] = static_cast<float>(value[0]);
    data[1] = static_cast<float>(value[1]);
    data[2] = static_cast<float>(value[2]);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_PARTIOFILE_H
