
#include "kernel.cuh"

extern "C"
__global__ void sum_kernel(float* a, float* b, float* c, int N)
{
    int idx = threadIdx.x + blockIdx.x * blockDim.x;

    if (idx < N)
        c[idx] = a[idx] + b[idx];
}
