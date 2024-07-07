#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    // Integer energy value (using 64-bit integer)
    int64_t local_energy_int = rank + 1; // Example local value
    int64_t global_energy_int;

    // Floating point energy value
    double local_energy_float = (rank + 1) * 1.5; // Example local value
    double global_energy_float;

    // Perform the MPI Allreduce operation for integer
    MPI_Allreduce(&local_energy_int, &global_energy_int, 1, MPI_INT64_T, MPI_SUM, MPI_COMM_WORLD);

    // Perform the MPI Allreduce operation for floating point
    MPI_Allreduce(&local_energy_float, &global_energy_float, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    if (rank == 0) {
        printf("Global energy (integer): %ld\n", global_energy_int);
        printf("Global energy (float): %f\n", global_energy_float);
    }

    MPI_Finalize();
    return 0;
}
