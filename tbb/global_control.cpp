#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"
#define TBB_PREVIEW_GLOBAL_CONTROL 1
#include "tbb/global_control.h"

using namespace tbb;

void foo()
{
    // The following code could use up to 16 threads.
    task_scheduler_init tsi(16);
    parallel_for( . . . );
}

void bar()
{
    // The following code could use up to 8 threads.
    task_scheduler_init tsi(8);
    parallel_for( . . . );
}

int main()
{
    {
        const size_t parallelism = task_scheduler_init::default_num_threads();
        // total parallelism that TBB can utilize is cut in half for the dynamic extension
        // of the given scope, including calls to foo() and bar()
        global_control c(global_control::max_allowed_parallelism, parallelism/2);
        foo();
        bar();
    } // restore previous parallelism limitation, if one existed
}
