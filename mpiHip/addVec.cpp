#include <mpi.h>
#include <hip/hip_runtime.h>
#include <iostream>

// Kernel function to be executed on the GPU
__global__ void vector_add(float* A, float* B, float* C, int N) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < N) {
        C[idx] = A[idx] + B[idx];
    }
}

int main(int argc, char* argv[]) {
    // Initialize MPI
    MPI_Init(&argc, &argv);

    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

    // Size of the vectors
    int N = 1024;

    // Allocate host memory
    float *h_A = new float[N];
    float *h_B = new float[N];
    float *h_C = new float[N];

    // Initialize vectors
    for (int i = 0; i < N; ++i) {
        h_A[i] = i;
        h_B[i] = i * 2;
    }

    // Allocate device memory
    float *d_A, *d_B, *d_C;
    hipMalloc(&d_A, N * sizeof(float));
    hipMalloc(&d_B, N * sizeof(float));
    hipMalloc(&d_C, N * sizeof(float));

    // Copy data from host to device
    hipMemcpy(d_A, h_A, N * sizeof(float), hipMemcpyHostToDevice);
    hipMemcpy(d_B, h_B, N * sizeof(float), hipMemcpyHostToDevice);

    // Launch kernel
    int threads_per_block = 256;
    int blocks_per_grid = (N + threads_per_block - 1) / threads_per_block;
    hipLaunchKernelGGL(vector_add, dim3(blocks_per_grid), dim3(threads_per_block), 0, 0, d_A, d_B, d_C, N);

    // Copy result back to host
    hipMemcpy(h_C, d_C, N * sizeof(float), hipMemcpyDeviceToHost);

    // Each process prints its portion of the result
    std::cout << "Process " << world_rank << " results: ";
    for (int i = 0; i < N; ++i) {
        std::cout << h_C[i] << " ";
    }
    std::cout << std::endl;

    // Free device memory
    hipFree(d_A);
    hipFree(d_B);
    hipFree(d_C);

    // Free host memory
    delete[] h_A;
    delete[] h_B;
    delete[] h_C;

    // Finalize MPI
    MPI_Finalize();

    return 0;
}
