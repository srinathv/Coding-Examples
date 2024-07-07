#include <mpi.h>
#include <stdio.h>
#include <iostream>


void globalSum_debug(int& x, int numpe, int mype) {
    if (numpe == 1) return;
    int y = 0;
    int mpi_err;
    printf(" My PE: %d and x is %d \n", mype, x);
    mpi_err=MPI_Allreduce(&x, &y, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    printf(" My PE: %d and y is %d AND MPI_allreduce error is %d \n", mype, y, mpi_err);
    x = y;
    printf(" My PE: %d AND NOE x is %d \n", mype, x);
}

void globalSum_debug_64(int64_t& x, int numpe, int mype) {
    if (numpe == 1) return;
    int64_t y=0;
    int mpi_err;
    printf(" D64 My PE: %d and x is %ld \n", mype, x);
    mpi_err=MPI_Allreduce(&x, &y, 1, MPI_INT64_T, MPI_SUM, MPI_COMM_WORLD);
    printf(" D64 My PE: %d and y is %ld AND MPI_allreduce error is %d \n", mype, y, mpi_err);
    x = y;
    printf(" D64 My PE: %d AND NOE x is %ld \n", mype, x);
}
int main(int argc, char** argv) {
    // Initialize the MPI environment
    MPI_Init(NULL, NULL);

    
    // Get the number of processes
    int world_size;
    int mpi_err;
    mpi_err=MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    std::cout << "MPI_comm_size error code " << mpi_err << "\n";
    // Get the rank of the process
    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

    // Get the name of the processor
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int name_len;
    MPI_Get_processor_name(processor_name, &name_len);

    // Print off a hello world message
    printf("Hello world from processor %s, rank %d"
           " out of %d processors\n",
           processor_name, world_rank, world_size);
//      printf("%d \n",world_rank);
    //printf("%d",world_rank); //may not want return character because of file name
    

    int nums=5372136;
    int64_t nums_64t=5372136;
    globalSum_debug(nums, world_size, world_rank);
    for (int i= 0; i < 1000 ; i++) {
    globalSum_debug_64(nums_64t, world_size, world_rank);
    }

    // Finalize the MPI environment.
    MPI_Finalize();
}



