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
* LAST REVISED: 02/15/17
*  ** S. Vadlamani: Serializing with a function + lambda expression use
*
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#if defined (__USE_TBB)
#include "tbb/tbb.h"
#include "tbb/task_scheduler_init.h"

#include <thread>
#include "tbb/enumerable_thread_specific.h"
#include "ParseCommandLine.h"
using namespace tbb;

//Used to handle identifying and handling number of threads
#include <tbb/task_scheduler_observer.h>
class concurrency_tracker: public tbb::task_scheduler_observer {
    tbb::atomic<int> num_threads;
public:
    concurrency_tracker() : num_threads() { observe(true); }
    /*override*/ void on_scheduler_entry( bool ) { ++num_threads; }
    /*override*/ void on_scheduler_exit( bool ) { --num_threads; }

    int get_concurrency() { return num_threads; }
};

#endif

#ifdef __USE_TAU
#include <TAU.h>
#endif

using namespace std;


#define NRA 10                 /* number of rows in matrix A */
#define NCA 10                 /* number of columns in matrix A */
#define NCB NRA                 /* number of columns in matrix B */
#define MASTER 0               /* taskid of first task */
#define FROM_MASTER 1          /* setting a message type */
#define FROM_WORKER 2          /* setting a message type */


/*************sub matrix multipy **********************/

void columMultipy(size_t i, int nca, int ncb, double a[][NCA],double b[][NCB], double c[][NCB])
{
  for (size_t j=0; j<ncb; j++) {
    for (size_t k=0; k<nca; k++)
      c[i][j] += a[i][k] * b[k][j];
    }
};

void serialApplyColumnMultiply( int nca, int ncb, int rows, double a[][NCA],double b[][NCB], double c[][NCB])
{
  for (size_t i = 0 ; i < rows; i++ )
    columMultipy(i, nca, ncb, a, b, c);
};

#if defined (__USE_TBB)
void tbb_SubMatrixMultiply(int nca, int ncb, int rows, double a[][NCA], double b[][NCB], double c[][NCB]){
	  parallel_for( blocked_range<size_t>(0,nca), [=](const blocked_range<size_t>& r){
#if defined(__USE_TAU)
TAU_PROFILE("inside tbb_SubMatrixMultiply loop","",TAU_DEFAULT);
#endif
	std::cout << "This threadID inside parallel_for is " << tbb::this_tbb_thread::get_id() << std::endl;
      for (size_t i=r.begin(); i!=r.end(); i++){
					for (size_t j=0; j<ncb; j++)
					{
						c[i][j] = 0.0;
						for (size_t k=0; k<nca; k++)
							c[i][j] += a[i][k] * b[k][j];
					}
      }
			});
}

void tbbApplyColumnMultiply(int nca, int ncb, int rows, double a[][NCA], double b[][NCB], double c[][NCB]){
  parallel_for( blocked_range<size_t>(0,nca), [=](const blocked_range<size_t>& r){
#if defined(__USE_TAU)
TAU_PROFILE("inside tbb_SubMatrixMultiply loop","",TAU_DEFAULT);
#endif
std::cout << "This threadID inside parallel_for is " << tbb::this_tbb_thread::get_id() << std::endl;
    for (size_t i=r.begin(); i!=r.end(); i++){
      columMultipy(i, nca, ncb, a, b, c);
    }
  });
}

#else
void subMatrixMultiply(int nca, int ncb, int rows, double a[][NCA], double b[][NCB], double c[][NCB])
{
 for (size_t i=0; i<rows; i++) {
         for (size_t j=0; j<ncb; j++) {
	//		c[i][j] = 0.0;
			for (size_t k=0; k<nca; k++)
      {
				c[i][j] += a[i][k] * b[k][j];
      }
	 }
 }
};
#endif


/******************/

int main (int argc, char *argv[])
{
	int rows = NRA,                  /* rows of matrix A sent to each worker */
    i, j, k, rc;           /* misc */
double	a[NRA][NCA],           /* matrix A to be multiplied */
	b[NCA][NCB],           /* matrix B to be multiplied */
	c[NRA][NCB];           /* result matrix C */

  #if defined (__USE_TBB)
  ParseCommandLine cmd("mpi_tbb_mm");
  cmd.addoption("threads", ParseCommandLine::INT, "Number of threads to use (default number cores + ht)", 1);
  #endif

  #if defined (__USE_TAU)
  TAU_PROFILE("main","",TAU_DEFAULT);
  #endif


  #if defined (__USE_TBB)
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
  std::cout << "outside of parallel_for loop, the ThreadId is " << tbb::this_tbb_thread::get_id() << std::endl;
  #endif

  printf("Initializing arrays...\n");
  for (i=0; i<NRA; i++)
    for (j=0; j<NCA; j++)
        a[i][j]= i+j;
  for (i=0; i<NCA; i++)
    for (j=0; j<NCB; j++)
        b[i][j]= i*j;
  for ( i=0; i<NCA; i++)
    for ( j=0; j<NCB; j++)
  		c[i][j] = 0.0;

#if defined(__USE_TBB)
#if defined(__USE_CLASS)
      parallel_for(blocked_range<int>(0,nca), Multiply());
#elif defined(__USE_TBB_FUNC)
      tbbApplyColumnMultiply(NCA,NCB,rows,a,b,c);
#else
			tbb_SubMatrixMultiply(NCA,NCB,rows,a,b,c);
#endif
#else
#if defined (__USE_FUNC)
      serialApplyColumnMultiply(NCA,NCB,rows,a,b,c);
      std::cout << " ran serialApplyColumnMultiply " << std::endl ;
#else
			subMatrixMultiply(NCA,NCB,rows,a,b,c);
      std::cout << " ran subMatrixMultiply " << std::endl ;
#endif
#endif
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

	 return 0;
}
