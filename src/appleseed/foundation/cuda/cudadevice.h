
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

// CUDA headers.
#include <cuda.h>

// Standard headers.
#include <cstddef>
#include <string>

namespace foundation
{

class CUDADevice
{
  public:
    // Constructor.
    explicit CUDADevice(const CUdevice device_number);

    // Device properties.

    CUdevice        m_cuda_device;
    std::string     m_name;
    int             m_compute_capability_major;
    int             m_compute_capability_minor;
    int             m_compute_mode;

    size_t          m_total_mem;

    int             m_max_threads_per_block;
    int             m_max_block_dim_x;
    int             m_max_block_dim_y;
    int             m_max_block_dim_z;
    int             m_max_grid_dim_x;
    int             m_max_grid_dim_y;
    int             m_max_grid_dim_z;

    int             m_max_registers;
};

class CUDADeviceList
  : public NonCopyable
{
  public:
    static const CUDADeviceList& instance();

    ~CUDADeviceList();

    bool empty() const;

    size_t size() const;

    const CUDADevice& get_device(const size_t device_number) const;

    CUcontext get_primary_context(const CUDADevice& device) const;

  private:
    struct Impl;
    Impl* impl;

    CUDADeviceList();
};

}   // namespace foundation
