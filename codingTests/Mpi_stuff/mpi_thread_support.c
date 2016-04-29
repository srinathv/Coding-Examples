//from llnl mpi tutorial page
#include "mpi.h"
#include <stdio.h>
int int main(int argc, char const *argv[]) {
  int provided, claimed;
  /*** Selec on of the following
  MPI_Init_thread( 0, 0, MPI_THREAD_SINGLE, &provided);
  MPI_Init_thread(0,0, MPI_THREAD_FUNNELED, &provided);
  MPI_Init_thread(0,0,MPI_THREAD_SERIALIZED, &provided);
  MPI_Init_thread(0,0, MPI_THREAD_MULTIPLE, &provided);
  ***/

  MPI_Init_thread (0,0, MPI_THREAD_MULTIPLE, &provided);
  MPI_Query_thread (&claimed);
    printf("Query thread level = %d   Init_thread level = %d\n", );
  MPI_Finalize();   
  return 0;
}
