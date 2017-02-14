/******************************************************************************
* FILE: mpi_mm.c
* DESCRIPTION:
*   MPI Matrix Multiply - C Version
*   In this code, the master task distributes a matrix multiply
*   operation to numtasks-1 worker tasks.
*   NOTE:  C and Fortran versions of this code differ because of the way
*   arrays are stored/passed.  C arrays are row-major order but Fortran
*   arrays are column-major order.
* AUTHOR: Blaise Barney. Adapted from Ros Leibensperger, Cornell Theory
*   Center. Converted to MPI: George L. Gusciora, MHPCC (1/95)
* LAST REVISED: 04/13/05
******************************************************************************/
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

#include "tbb/tbb.h"
#include "tbb/task_scheduler_init.h"

#include <thread>
#include "tbb/enumerable_thread_specific.h"
#include "ParseCommandLine.h"

#ifdef __USE_TAU
#include <TAU.h>
#endif


using namespace tbb;
using namespace std;

#include <tbb/task_scheduler_observer.h>
class concurrency_tracker: public tbb::task_scheduler_observer {
    tbb::atomic<int> num_threads;
public:
    concurrency_tracker() : num_threads() { observe(true); }
    /*override*/ void on_scheduler_entry( bool ) { ++num_threads; }
    /*override*/ void on_scheduler_exit( bool ) { --num_threads; }

    int get_concurrency() { return num_threads; }
};




#define NRA 15                 /* number of rows in matrix A */
#define NCA 15                 /* number of columns in matrix A */
#define NCB 15                  /* number of columns in matrix B */
#define MASTER 0               /* taskid of first task */
#define FROM_MASTER 1          /* setting a message type */
#define FROM_WORKER 2          /* setting a message type */


/*************sub matrix multipy **********************/

class Multiply
{
public:
  void operator()(blocked_range<int> r) const {
    std::cout << "This threadID inside parallel_for is " << tbb::this_tbb_thread::get_id() << std::endl;
#if defined (__USE_TAU)
    TAU_PROFILE("inside Multiply class","",TAU_DEFAULT);
#endif
for (int i = r.begin(); i != r.end(); ++i) {
for (size_t k=0; k<ncb; k++) {
	 for (size_t i=0; i<rows; i++) {
			c[i][k] = 0.0;
			for (size_t j=0; j<nca; j++)
				 c[i][k] = c[i][k] + a[i][j] * b[j][k];
	 }
}

void subMatrixMultiply(int nca, int ncb, int rows, double a[][NCA], double b[][NCB], double c[][NCB])
{
for (size_t k=0; k<ncb; k++)
	 for (size_t i=0; i<rows; i++)
	 {
			c[i][k] = 0.0;
			for (size_t j=0; j<nca; j++)
				 c[i][k] += a[i][j] * b[j][k];
	 }
};

void tbb_SubMatrixMultiply(int nca, int ncb, int rows, double a[][NCA], double b[][NCB], double c[][NCB]){
	  parallel_for( 0,ncb, [&](int k){
#if defined(__USE_TAU)
TAU_PROFILE("inside tbb_SubMatrixMultiply loop","",TAU_DEFAULT);
#endif
	std::cout << "This threadID inside parallel_for is " << tbb::this_tbb_thread::get_id() << std::endl;

					 for (size_t i=0; i<rows; i++)
					 {
							c[i][k] = 0.0;
							for (size_t j=0; j<nca; j++)
								 c[i][k] += a[i][j] * b[j][k];
					 }
			});

}

/******************/

int main (int argc, char *argv[])
{
int	numtasks,              /* number of tasks in partition */
	taskid,                /* a task identifier */
	numworkers,            /* number of worker tasks */
	source,                /* task id of message source */
	dest,                  /* task id of message destination */
	mtype,                 /* message type */
	rows,                  /* rows of matrix A sent to each worker */
	averow, extra, offset, /* used to determine rows sent to each worker */
	i, j, k, rc;           /* misc */
double	a[NRA][NCA],           /* matrix A to be multiplied */
	b[NCA][NCB],           /* matrix B to be multiplied */
	c[NRA][NCB];           /* result matrix C */
MPI_Status status;

ParseCommandLine cmd("mpi_tbb_mm");
cmd.addoption("threads", ParseCommandLine::INT, "Number of threads to use (default number cores + ht)", 1);

MPI_Init(&argc,&argv);
MPI_Comm_rank(MPI_COMM_WORLD,&taskid);
MPI_Comm_size(MPI_COMM_WORLD,&numtasks);
if (numtasks < 2 ) {
  printf("Need at least two MPI tasks. Quitting...\n");
  MPI_Abort(MPI_COMM_WORLD, rc);
  exit(1);
  }
#if defined (__USE_TAU)
TAU_PROFILE("main","",TAU_DEFAULT);
#endif

numworkers = numtasks-1;
//int n = task_scheduler_init::default_num_threads();

#if defined (__LIKE_GRAVIT)
  cmd.parse(argc, argv);

tbb::task_scheduler_init* init;
if (!cmd.isSet("threads")) {
  init = new tbb::task_scheduler_init(std::thread::hardware_concurrency());
  std::cout << "Initialized mpi_tbb_mm with " << std::thread::hardware_concurrency() <<
  " threads..."<< std::endl;
} else {
  init = new tbb::task_scheduler_init(cmd.get<int>("threads"));
  std::cout << "Initialized mpi_tbb_mm with " << cmd.get<int>("threads") <<
    " threads..."<< std::endl;
}
#else
//tbb::task_scheduler_init init;  // Automatic number of threads
tbb::task_scheduler_init init(tbb::task_scheduler_init::default_num_threads());  // Explicit number of threads
#endif

// Report thread id's
std::cout << "outside of parallel_for loop, the ThreadId is " << tbb::this_tbb_thread::get_id() << std::endl;
/**************************** master task ************************************/
   if (taskid == MASTER)
   {
      printf("mpi_mm has started with %d tasks.\n",numtasks);
      printf("Initializing arrays...\n");
      for (i=0; i<NRA; i++)
         for (j=0; j<NCA; j++)
            a[i][j]= i+j;
      for (i=0; i<NCA; i++)
         for (j=0; j<NCB; j++)
            b[i][j]= i*j;

      /* Send matrix data to the worker tasks */
      averow = NRA/numworkers;
      extra = NRA%numworkers;
      offset = 0;
      mtype = FROM_MASTER;
      for (dest=1; dest<=numworkers; dest++)
      {
         rows = (dest <= extra) ? averow+1 : averow;
         printf("Sending %d rows to task %d offset=%d\n",rows,dest,offset);
         MPI_Send(&offset, 1, MPI_INT, dest, mtype, MPI_COMM_WORLD);
         MPI_Send(&rows, 1, MPI_INT, dest, mtype, MPI_COMM_WORLD);
         MPI_Send(&a[offset][0], rows*NCA, MPI_DOUBLE, dest, mtype,
                   MPI_COMM_WORLD);
         MPI_Send(&b, NCA*NCB, MPI_DOUBLE, dest, mtype, MPI_COMM_WORLD);
         offset = offset + rows;
      }

      /* Receive results from worker tasks */
      mtype = FROM_WORKER;
      for (i=1; i<=numworkers; i++)
      {
         source = i;
         MPI_Recv(&offset, 1, MPI_INT, source, mtype, MPI_COMM_WORLD, &status);
         MPI_Recv(&rows, 1, MPI_INT, source, mtype, MPI_COMM_WORLD, &status);
         MPI_Recv(&c[offset][0], rows*NCB, MPI_DOUBLE, source, mtype,
                  MPI_COMM_WORLD, &status);
         printf("Received results from task %d\n",source);
      }

      /* Print results */
      printf("******************************************************\n");
      printf("Result Matrix:\n");
      for (i=0; i<NRA; i++)
      {
         printf("\n");
         for (j=0; j<NCB; j++)
            printf("%6.2f   ", c[i][j]);
      }
      printf("\n******************************************************\n");
      printf ("Done.\n");
   }


/**************************** worker task ************************************/
   if (taskid > MASTER)
   {
#ifdef __USE_TAU
TAU_PROFILE("worker tasks","",TAU_DEFAULT);
#endif

      mtype = FROM_MASTER;
      MPI_Recv(&offset, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD, &status);
      MPI_Recv(&rows, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD, &status);
      MPI_Recv(&a, rows*NCA, MPI_DOUBLE, MASTER, mtype, MPI_COMM_WORLD, &status);
      MPI_Recv(&b, NCA*NCB, MPI_DOUBLE, MASTER, mtype, MPI_COMM_WORLD, &status);

      // for (k=0; k<NCB; k++)
      //    for (i=0; i<rows; i++)
      //    {
      //       c[i][k] = 0.0;
      //       for (j=0; j<NCA; j++)
      //          c[i][k] = c[i][k] + a[i][j] * b[j][k];
      //    }

			//subMatrixMultiply(NCA,NCB,rows,a,b,c);
			tbb_SubMatrixMultiply(NCA,NCB,rows,a,b,c);


      mtype = FROM_WORKER;
      MPI_Send(&offset, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD);
      MPI_Send(&rows, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD);
      MPI_Send(&c, rows*NCB, MPI_DOUBLE, MASTER, mtype, MPI_COMM_WORLD);
   }
   MPI_Finalize();
	 return 0;
}
