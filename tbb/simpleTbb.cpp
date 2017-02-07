#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"
#include <iostream>
#include <vector>

struct mytask {
  mytask(size_t n)
    :_n(n)
  {}
  void operator()() {
    for (int i=0;i<1000000;++i) {}  // Deliberately run slow
    std::cerr << "[" << _n << "]";
  }
  size_t _n;
};


/*********************  MAIN ********************/
int main(int,char**) {



#if defined (__LIKE_GRAVIT)
  cmd.parse(argc, argv);

tbb::task_scheduler_init* init;
if (!cmd.isSet("threads")) {
  init = new tbb::task_scheduler_init(std::thread::hardware_concurrency());
  std::cout << "Initialized GraviT with " << std::thread::hardware_concurrency() <<
  " threads..."<< std::endl;
} else {
  init = new tbb::task_scheduler_init(cmd.get<int>("threads"));
  std::cout << "Initialized GraviT with " << cmd.get<int>("threads") <<
    " threads..."<< std::endl;
}
#else
//tbb::task_scheduler_init init;  // Automatic number of threads
tbb::task_scheduler_init init(tbb::task_scheduler_init::default_num_threads());  // Explicit number of threads
#endif


  std::vector<mytask> tasks;
  for (int i=0;i<1000;++i)
    tasks.push_back(mytask(i));

  tbb::parallel_for(
    tbb::blocked_range<size_t>(0,tasks.size()),
    [&tasks](const tbb::blocked_range<size_t>& r) {
      for (size_t i=r.begin();i<r.end();++i) tasks[i]();
    }
  );

  std::cerr << std::endl;

  return 0;
}
