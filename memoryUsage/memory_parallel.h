#include <mpi.h>
#include <memory.h>

int get_cluster_memory_usage_kb(long* vmrss_per_process, long* vmsize_per_process, int root, int np);
int get_global_memory_usage_kb(long* global_vmrss, long* global_vmsize, int np);

