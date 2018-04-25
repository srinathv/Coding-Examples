#include <mpi.h>
#include <stdio.h>
#include <unistd.h>
#include "memory_parallel.h"

int main (int argc, char *argv[])
{
    int id, np;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    char hostname[MPI_MAX_PROCESSOR_NAME];
    int processor_name_len;
    int j,k ;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &np);
    MPI_Comm_rank(MPI_COMM_WORLD, &id);
    MPI_Get_processor_name(processor_name, &processor_name_len);

    printf("Number_of_processes=%03d, My_rank=%03d, processor_name=%5s\n",
        np, id, processor_name);

    int entrySize = 1000000 + id * 100000;

    long* l_buffer[entrySize];

    //for (int j = 0; j < entrySize; j++)
    for (j = 0; j < entrySize; j++)
    {
        l_buffer[j] = 0;
    }

    long vmrss_per_process[np];
    long vmsize_per_process[np];
    get_cluster_memory_usage_kb(vmrss_per_process, vmsize_per_process, 0, np);

    if (id == 0)
    {
        //for (int k = 0; k < np; k++)
        for (k = 0; k < np; k++)
        {
            printf("Process %03d: VmRSS = %6ld KB, VmSize = %6ld KB\n",
                k, vmrss_per_process[k], vmsize_per_process[k]);
        }
    }

    long global_vmrss, global_vmsize;
    get_global_memory_usage_kb(&global_vmrss, &global_vmsize, np);
    if (id == 0)
    {
        printf("Global memory usage: VmRSS = %6ld KB, VmSize = %6ld KB\n",
            global_vmrss, global_vmsize);
    }

    MPI_Finalize();
    return 0;
}
