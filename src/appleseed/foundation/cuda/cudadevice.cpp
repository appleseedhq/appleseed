
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

// Interface header.
#include "cudadevice.h"

// appleseed.foundation headers.
#include "foundation/cuda/exception.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <vector>

namespace foundation
{

//
// CUDADevice class implementation.
//

CUDADevice::CUDADevice(const CUdevice device_number)
  : m_cuda_device(device_number)
{
    char device_name[256];
    check_cuda_result(cuDeviceGetName(device_name, 256, m_cuda_device));
    m_name = device_name;

    check_cuda_result(cuDeviceGetAttribute(&m_compute_capability_major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, m_cuda_device));
    check_cuda_result(cuDeviceGetAttribute(&m_compute_capability_minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, m_cuda_device));
    check_cuda_result(cuDeviceGetAttribute(&m_compute_mode, CU_DEVICE_ATTRIBUTE_COMPUTE_MODE, m_cuda_device));

    check_cuda_result(cuDeviceTotalMem(&m_total_mem, m_cuda_device));

    check_cuda_result(cuDeviceGetAttribute(&m_max_threads_per_block, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, m_cuda_device));

    check_cuda_result(cuDeviceGetAttribute(&m_max_block_dim_x, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X, m_cuda_device));
    check_cuda_result(cuDeviceGetAttribute(&m_max_block_dim_y, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y, m_cuda_device));
    check_cuda_result(cuDeviceGetAttribute(&m_max_block_dim_z, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z, m_cuda_device));

    check_cuda_result(cuDeviceGetAttribute(&m_max_grid_dim_x, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X, m_cuda_device));
    check_cuda_result(cuDeviceGetAttribute(&m_max_grid_dim_y, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y, m_cuda_device));
    check_cuda_result(cuDeviceGetAttribute(&m_max_grid_dim_z, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z, m_cuda_device));

    check_cuda_result(cuDeviceGetAttribute(&m_max_registers, CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK, m_cuda_device));
}


//
// CUDADeviceList class implementation.
//

struct CUDADeviceList::Impl
{
    std::vector<CUDADevice>         m_devices;
    mutable std::vector<CUcontext>  m_contexts;

    Impl()
    {
        // Initialize CUDA.
        check_cuda_result(cuInit(0));

        int device_count;
        check_cuda_result(cuDeviceGetCount(&device_count));

        m_devices.reserve(static_cast<size_t>(device_count));
        m_contexts.resize(static_cast<size_t>(device_count), nullptr);

        for (int i = 0; i < device_count; ++i)
            m_devices.emplace_back(i);
    }

    ~Impl()
    {
        for (size_t i = 0, e = m_contexts.size(); i < e; ++i)
        {
            if (m_contexts[i] != nullptr)
                cuDevicePrimaryCtxRelease(static_cast<CUdevice>(i));
        }
    }
};

CUDADeviceList::CUDADeviceList()
  : impl(new Impl())
{
}

CUDADeviceList::~CUDADeviceList()
{
    delete impl;
}

const CUDADeviceList& CUDADeviceList::instance()
{
    static CUDADeviceList x;
    return x;
}

bool CUDADeviceList::empty() const
{
    return impl->m_devices.empty();
}

size_t CUDADeviceList::size() const
{
    return impl->m_devices.size();
}

const CUDADevice& CUDADeviceList::get_device(const size_t device_number) const
{
    assert(device_number < size());
    return impl->m_devices[device_number];
}

CUcontext CUDADeviceList::get_primary_context(const CUDADevice& device) const
{
    assert(device.m_cuda_device < size());
    assert(device.m_compute_mode != CU_COMPUTEMODE_PROHIBITED);

    const CUdevice dev = device.m_cuda_device;

    // Create a primary context for the device if needed.
    if (impl->m_contexts[dev] == nullptr)
    {
        CUcontext ctx;
        check_cuda_result(cuDevicePrimaryCtxRetain(&ctx, dev));

        // todo: set context flags here?
        // cuDevicePrimaryCtxSetFlags(dev, ...);

        check_cuda_result(cuCtxPushCurrent(ctx));
        impl->m_contexts[dev] = ctx;
    }

    return impl->m_contexts[dev];
}

}   // namespace foundation
