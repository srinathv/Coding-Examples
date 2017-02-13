#include <cstdio>
#include <utility>

#include "tbb/task_scheduler_init.h"
#include "tbb/enumerable_thread_specific.h"
#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"

typedef tbb::enumerable_thread_specific< std::pair<int,int> > CounterType;
CounterType MyCounters (std::make_pair(0,0));

struct Body {
     void operator()(const tbb::blocked_range<int> &r) const {
          CounterType::reference my_counter = MyCounters.local();
          ++my_counter.first;
          for (int i = r.begin(); i != r.end(); ++i)
              ++my_counter.second;
     }
};

int main() {
     tbb::parallel_for( tbb::blocked_range<int>(0, 100000000), Body());

     for (CounterType::const_iterator i = MyCounters.begin();
          i != MyCounters.end(); ++i)
     {
            printf("Thread stats:\n");
            printf("     calls to operator(): %d", i->first);
            printf("     total # of iterations executed: %d\n\n",
                   i->second);
    }
}
