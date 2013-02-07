
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Esteban Tovagliari, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_UI_VIEWPORT_H
#define APPLESEED_FOUNDATION_UI_VIEWPORT_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

namespace foundation
{

//
// 2-dimensional viewport.
//

template <typename T>
class Viewport
{
  public:
    // Types.
    typedef Vector<T, 2> RealVectorType;
    typedef Vector2i IntVectorType;
    typedef AABB<T, 2> RealBoxType;
    typedef AABB2i IntBoxType;

    Viewport();
    explicit Viewport(const bool y_down);

    bool y_down() const;

    const RealBoxType& world() const;
    const IntBoxType& device() const;

    T zoom_x() const;
    T zoom_y() const;

    RealVectorType device_to_world(const IntVectorType& p) const;
    IntVectorType world_to_device(const RealVectorType& p) const;

    RealVectorType device_to_world_vector(const RealVectorType& v) const;
    RealBoxType device_to_world(const IntBoxType& b) const;
    IntBoxType world_to_device(const RealBoxType& b) const;

    void reset();
    void reset(const int w, const int h);
    void reset(const IntBoxType& device);
    void reset(const IntBoxType& device, const RealBoxType& world);

    void resize(const IntBoxType& device);
    void resize(const int w, const int h);

    void scroll(const IntVectorType& inc);
    void scroll_to_center_point(const RealVectorType& center);

    void zoom(const RealVectorType& center, const float factor);
    void zoom(const RealVectorType& center, const float xfactor, const float yfactor);

  private:
    const bool  m_y_down;
    IntBoxType  m_device;
    RealBoxType m_world;
};


//
// Full specializations float and double.
//

typedef Viewport<float>  Viewportf;
typedef Viewport<double> Viewportd;


//
// 2-dimensional viewport implementation.
//

template <typename T>
inline Viewport<T>::Viewport()
  : m_y_down(false)
{
}

template <typename T>
inline Viewport<T>::Viewport(const bool y_down)
  : m_y_down(y_down)
{
}

template <typename T>
inline bool Viewport<T>::y_down() const
{
    return m_y_down;
}

template <typename T>
inline const typename Viewport<T>::RealBoxType& Viewport<T>::world() const
{
    return m_world;
}

template <typename T>
inline const typename Viewport<T>::IntBoxType& Viewport<T>::device() const
{
    return m_device;
}

template <typename T>
inline T Viewport<T>::zoom_x() const
{
    return m_device.extent(0) / m_world.extent(0);
}

template <typename T>
inline T Viewport<T>::zoom_y() const
{
    return m_device.extent(1) / m_world.extent(1);
}

template <typename T>
inline typename Viewport<T>::RealVectorType Viewport<T>::device_to_world(const IntVectorType& p) const
{
    int y = p[1];

    if (y_down())
        y = m_device.min[1] + (m_device.max[1] - y);

    return RealVectorType((p[0] - m_device.min[0]) / zoom_x() + m_world.min[0],
                          (y - m_device.min[1]) / zoom_y() + m_world.min[1]);
}

template <typename T>
inline typename Viewport<T>::IntVectorType Viewport<T>::world_to_device(const RealVectorType& p) const
{
    int x = (p[0] - m_world.min[0]) * zoom_x() + m_device.min[0];
    int y = (p[1] - m_world.min[1]) * zoom_y() + m_device.min[1];

    if (y_down())
        y = m_device.min[1] + (m_device.max[1] - y);

    return IntVectorType(x, y);
}

template <typename T>
inline typename Viewport<T>::RealVectorType Viewport<T>::device_to_world_vector(const RealVectorType& v) const
{
    return RealVectorType(v[0] / zoom_x(),
                          v[1] / zoom_y());
}

template <typename T>
inline typename Viewport<T>::RealBoxType Viewport<T>::device_to_world(const IntBoxType& b) const
{
    return RealBoxType(device_to_world(IntVectorType(b.min[0], b.min[1])),
                       device_to_world(IntVectorType(b.max[0], b.max[1])));
}

template <typename T>
inline typename Viewport<T>::IntBoxType Viewport<T>::world_to_device(const RealBoxType& b) const
{
    return IntBoxType(world_to_device(RealVectorType(b.min[0], b.min[1])),
                      world_to_device(RealVectorType(b.max[0], b.max[1])));
}

template <typename T>
inline void Viewport<T>::reset()
{
    m_world = RealBoxType(IntVectorType(m_device.min[0], m_device.min[1]),
                          IntVectorType(m_device.max[0], m_device.max[1]));
}

template <typename T>
inline void Viewport<T>::reset(const int w, const int h)
{
    reset(IntBoxType(IntVectorType(0, 0), IntVectorType(w - 1, h - 1)));
}

template <typename T>
inline void Viewport<T>::reset(const IntBoxType& device)
{
    m_device = device;
    reset();
}

template <typename T>
inline void Viewport<T>::reset(const IntBoxType& device, const RealBoxType& world)
{
    m_world = world;
    m_device = device;
}

template <typename T>
inline void Viewport<T>::resize(const IntBoxType& device)
{
    m_world.max[0] = m_world.min[0] + device.extent(0) / zoom_x();
    m_world.max[1] = m_world.min[1] + device.extent(1) / zoom_y();
    m_device = device;
}

template <typename T>
inline void Viewport<T>::resize(const int w, const int h)
{
    resize(IntBoxType(IntVectorType(0, 0), IntVectorType(w - 1, h - 1)));
}

template <typename T>
inline void Viewport<T>::scroll(const IntVectorType& inc)
{
    if (y_down())
        m_world = m_world.translate(RealVectorType(inc[0] / zoom_x(), -inc[1] / zoom_y()));
    else
        m_world = m_world.translate(RealVectorType(inc[0] / zoom_x(),  inc[1] / zoom_y()));
}

template <typename T>
inline void Viewport<T>::scroll_to_center_point(const RealVectorType& center)
{
    m_world = m_world.translate(center - m_world.center());
}

template <typename T>
inline void Viewport<T>::zoom(const RealVectorType& center, const float factor)
{
    zoom(center, factor, factor);
}

template <typename T>
inline void Viewport<T>::zoom(const RealVectorType& center, const float xfactor, const float yfactor)
{
    m_world = m_world.translate(RealVectorType(-center[0], -center[1]));
    m_world.min[0] *= xfactor;
    m_world.min[1] *= yfactor;
    m_world.max[0] *= xfactor;
    m_world.max[1] *= yfactor;
    m_world = m_world.translate(RealVectorType(center[0], center[1]));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UI_VIEWPORT_H
