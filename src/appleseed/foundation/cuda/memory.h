
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/cuda/exception.h"

// CUDA headers.
#include "cuda.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// Device memory allocators.
//

class GlobalDeviceMemoryAllocator
{
  public:
    typedef CUdeviceptr pointer_type;

    static pointer_type alloc(const size_t size, const unsigned int flags);

    static CUresult free(pointer_type p);

    static CUdeviceptr get_device_ptr(const pointer_type p)
    {
        return p;
    }
};


class ZeroCopyDeviceMemoryAllocator
{
  public:
    typedef void* pointer_type;

    static pointer_type alloc(const size_t size, const unsigned int flags);

    static CUresult free(pointer_type p);

    static CUdeviceptr get_device_ptr(const pointer_type p)
    {
        CUdeviceptr dev_p;
        check_cuda_result(cuMemHostGetDevicePointer(&dev_p, p, 0));
        return dev_p;
    }
};


class UnifiedDeviceMemoryAllocator
{
  public:
    typedef CUdeviceptr pointer_type;

    static pointer_type alloc(const size_t size, const unsigned int flags);

    static CUresult free(pointer_type p);

    static CUdeviceptr get_device_ptr(const pointer_type p)
    {
        return p;
    }
};


//
// DevicePtr class.
//

template <typename T>
class DevicePtr
{
  public:
    explicit DevicePtr(T* ptr = nullptr)
      : m_ptr(ptr)
    {
    }

    T* get() const
    {
        return m_ptr;
    }

  private:
    T* m_ptr;
};


//
// DeviceBuffer class.
//

template <typename T, typename DeviceMemoryAllocator = GlobalDeviceMemoryAllocator>
class DeviceBuffer
  : public NonCopyable
{
  public:
    typedef typename DeviceMemoryAllocator::pointer_type pointer_type;

    explicit DeviceBuffer(const size_t size, const unsigned int flags = 0)
      : m_ptr(0)
      , m_size(0)
    {
        reset(size, flags);
    }

    ~DeviceBuffer()
    {
        DeviceMemoryAllocator::free(m_ptr);
    }

    void reset()
    {
        if (m_ptr)
        {
            check_cuda_result(DeviceMemoryAllocator::free(m_ptr));
            m_ptr = 0;
            m_size = 0;
        }
    }

    void reset(const size_t size, const unsigned int flags = 0)
    {
        reset();

        if (size != 0)
        {
            m_ptr = DeviceMemoryAllocator::alloc(size * sizeof(T), flags);
            m_size = size;
        }
    }

    size_t size() const
    {
        return m_size;
    }

    CUdeviceptr get_device_pointer() const
    {
        return DeviceMemoryAllocator::get_device_ptr(m_ptr);
    }

    const DevicePtr<T> get() const
    {
        return DevicePtr<T>(
            reinterpret_cast<T*>(
                DeviceMemoryAllocator::get_device_ptr(m_ptr)));
    }

  private:
    pointer_type    m_ptr;
    size_t          m_size;
};

}       // namespace foundation
