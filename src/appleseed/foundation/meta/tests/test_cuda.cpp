
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

// appleseed.foundation headers.
#include "foundation/cuda/cudadevice.h"
#include "foundation/cuda/exception.h"
#include "foundation/cuda/memory.h"
#include "foundation/cuda/module.h"
#include "foundation/utility/test.h"

// CUDA headers.
#include "cuda.h"

// Standard headers.
#include <algorithm>
#include <iostream>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Utility_CUDA)
{
    TEST_CASE(Scratchpad)
    {
        const CUDADeviceList& dev_list = CUDADeviceList::instance();

        if (dev_list.empty())
            return;

        std::cout << "Picking device..." << std::endl;;
        const CUDADevice& device = dev_list.get_device(0);

        std::cout << "Creating primary context on device..." << std::endl;;
        dev_list.get_primary_context(device);

        const int N = 8;

        // A vector.
        std::vector<float> host_a(N, 0);
        std::iota(host_a.begin(), host_a.end(), 0);

        DeviceBuffer<float> dev_a(N);
        check_cuda_error(cuMemcpyHtoD(
            dev_a.get_device_pointer(),
            host_a.data(),
            host_a.size() * sizeof(float)));

        // B vector.
        std::vector<float> host_b(N, 0);
        std::fill(host_b.begin(), host_b.end(), 32);

        DeviceBuffer<float> dev_b(N);
        check_cuda_error(cuMemcpyHtoD(
            dev_b.get_device_pointer(),
            host_b.data(),
            host_b.size() * sizeof(float)));

        // C vector.
        std::vector<float> host_c(N, 0);
        DeviceBuffer<float> dev_c(N);

        // Load module.
        CUDAModule module(
            "/hdd/Devel/appleseedhq/appleseedX/sandbox/kernels/fatbinkernel.fatbin");

        CUfunction kernel = module.get_function("sum_kernel");

        {
            int min_grid_size;
            int thread_block_size;

            cuOccupancyMaxPotentialBlockSize(
                &min_grid_size,
                &thread_block_size,
                kernel,
                nullptr,
                0,
                0);

            std::cout << "Min grid size = " << min_grid_size << std::endl;
            std::cout << "Thread block size = " << thread_block_size << std::endl;
        }

        // Launch kernel!
        {
            // Kernel arguments
            void* a_ptr = reinterpret_cast<void*>(dev_a.get_device_pointer());
            void* b_ptr = reinterpret_cast<void*>(dev_b.get_device_pointer());
            void* c_ptr = reinterpret_cast<void*>(dev_c.get_device_pointer());
            int size = N;

            void *args[] =
            {
                &a_ptr,
                &b_ptr,
                &c_ptr,
                &size
            };

            // Grid/Block configuration
            unsigned int threadsPerBlock = N;
            unsigned int blocksPerGrid = 1;

            // Launch the CUDA kernel
            check_cuda_error(
                cuLaunchKernel(
                    kernel,
                    blocksPerGrid, 1, 1,
                    threadsPerBlock, 1, 1,
                    0,
                    nullptr,
                    args,
                    nullptr));

            check_cuda_error(cuCtxSynchronize());
        }

        // Copy results to host.
        check_cuda_error(cuMemcpyDtoH(
            host_c.data(),
            dev_c.get_device_pointer(),
            host_c.size() * sizeof(float)));

// ...
for (size_t i = 0; i < host_a.size(); ++i)
    std::cout << host_a[i] << " ";
std::cout << std::endl;

for (size_t i = 0; i < host_b.size(); ++i)
    std::cout << host_b[i] << " ";
std::cout << std::endl;

for (size_t i = 0; i < host_c.size(); ++i)
    std::cout << host_c[i] << " ";
std::cout << std::endl;

        // Check results.
        std::vector<float> result(N, 0);
        std::iota(result.begin(), result.end(), host_b[0]);

        EXPECT_TRUE(host_c == result);
    }
}
