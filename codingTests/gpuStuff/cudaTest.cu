#include "stdio.h"

#define NUM_BLOCKS 32
#define NUM_THREADS_PER_BLOCK 32

// value is a pointer to a single integer
__global__ void testKernel(int* value) {
  if (blockIdx.x == 0 && threadIdx.x == 0) *value = 1;
}

int main(int argc, char** argv) {

  int value_cpu = 0; 

  int* value_gpu;

  cudaMalloc((void**)&value_gpu,sizeof(float));
  cudaMemcpy(value_gpu,&value_cpu,sizeof(float),cudaMemcpyHostToDevice);
 
  /* Call the gpu kernel */
  testKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(value_gpu);

  cudaMemcpy(&value_cpu,value_gpu,sizeof(float),cudaMemcpyDeviceToHost);
  if (value_cpu == 1) {
    printf("The cuda test passed GPU not corrupted\n");
    return 0;
  } else {
    printf("The cuda test failed the GPU is corrupted\n");
    return -1;
  }
}
