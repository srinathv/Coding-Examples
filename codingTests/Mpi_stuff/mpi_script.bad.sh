batch10> cat mk_nodecount.sh
#!/bin/bash

/bin/rm -f tau_mpisize.c tau_mpisize.exe

arch=`tau-config | grep TAUARCH | sed -e 's/TAUARCH=//'`
echo $arch

if [ "x$arch" = "xcraycnl" ];  then
    echo "ON CRAY"
      cat > tau_mpisize.c <<EOF
      #include <mpi.h>
      int main(int argc, char **argv)
      {
          int rank, size;
            MPI_Init(&argc, &argv);
              MPI_Comm_size(MPI_COMM_WORLD, &size);
                MPI_Comm_rank(MPI_COMM_WORLD, &rank);
                  if (rank == 0) {
                        printf("%d\n", size);
                          }
                            MPI_Finalize();
                              return size;
                            }
                            EOF
                            echo "compiling tau_mpisize"
                            cc tau_mpisize.c -o tau_mpisize.exe
                            echo "finished compiling tau_mpisize"
                            echo "running tau_mpisize.exe"
                            mpisize=`./tau_mpisize.exe`
                            echo "mpisize is $mpisize \n"

                            nodecount=$mpisize

                            echo "running prime_mpi.intel15.exe"
                            ./prime_mpi.intel15.exe

                          else
                              echo "NOT ON CRAY"
                            fi
