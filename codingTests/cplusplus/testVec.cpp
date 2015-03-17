
#include <iostream>
#include <boost/container/vector.hpp>
#include <boost/timer/timer.hpp>
#include <vector>
#ifdef USE_TAU
#include <TAU.h>
#endif

using std::vector;

int main(){
#ifdef USE_TAU
  TAU_START("main");
  TAU_PROFILE_SET_NODE(0);
#endif
  int N = 1000000000;
  std::cout << "This is vector push_back time" << std::endl;

  {   
    boost::timer::auto_cpu_timer t; 
    std::vector<float> v1;
#ifdef USE_TAU
  TAU_START("std::vector.push_back loop");
#endif
    for (int i = 0; i < N; ++i)
    {
        v1.push_back(float(i));
    }
#ifdef USE_TAU
  TAU_STOP("std::vector.push_back loop");
#endif
  }

  std::cout << "This is boost::container::vector push_back time" << std::endl;
  {
    boost::timer::auto_cpu_timer t;
    boost::container::vector<float> v3;
#ifdef USE_TAU
  TAU_START("boost::container::vector.push_back loop");
#endif
    for (int i = 0; i < N; ++i)
    {
        v3.push_back(float(i));
    }
#ifdef USE_TAU
  TAU_STOP("boost::container::vector.push_back loop");
#endif
  }

#ifdef USE_TAU
  TAU_STOP("main");
#endif

	return 0;
}


