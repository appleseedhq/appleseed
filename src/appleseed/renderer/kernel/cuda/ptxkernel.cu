
#include "vector_functions.h"

#include "foundation/image/color.h"
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/vector.h"
using namespace foundation;

#include "kernel.cuh"

__global__ void sum_kernel_builtin(float3* a, float3* b, float3* c)
{
    // Global thread index.
    int idx = threadIdx.x + blockIdx.x * blockDim.x;

    // Perform computations in entire thread.
    c[idx] = make_float3(
        a[idx].x + b[idx].x,
        a[idx].y + b[idx].y,
        a[idx].z + b[idx].z);
}

__global__ void sum_kernel_v3f(Vector3f* a, Vector3f* b, Vector3f* c)
{
    // Global thread index.
    int idx = threadIdx.x + blockIdx.x * blockDim.x;

    // Perform computations in entire thread.
    c[idx] = Vector3f(
        a[idx].x + b[idx].x,
        a[idx].y + b[idx].y,
        a[idx].z + b[idx].z);
}
