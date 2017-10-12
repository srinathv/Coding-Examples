#include <stdio.h>
#include <sched.h>
#include <omp.h>

int main() {
#pragma omp parallel
      {
       int thread_num = omp_get_thread_num();
       int cpu_num = sched_getcpu();
       int findmycpu_num = findmycpu_();
       printf("Thread %3d is running on CPU %3d\n", thread_num, cpu_num);
       printf("Thread %3d is running on myCPU %3d\n", thread_num, findmycpu_num);
      }

          return 0;
}
