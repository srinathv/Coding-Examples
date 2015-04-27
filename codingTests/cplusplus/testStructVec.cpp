
#include <iostream>
#include <boost/container/vector.hpp>
#include <boost/timer/timer.hpp>
#include <vector>
#ifdef USE_TAU
#include <TAU.h>
#endif
#ifdef USE_TBB
#include "tbb/concurrent_vector.h"
#endif


/* want to test vectors of structs
 * “skinny” ray:
        origin, point, three floats (often implemented as a Float4)
        direction, vector, three floats (often implemented as a Float4)
        distance, scalar, float

“fat” ray:
        origin, point, three floats (often implemented as a Float4)
        direction, vector, three floats (often implemented as a Float4)
        distance, scalar, float
        color, scalars, three floats (often a Float4)
        pixel, scalar, int
*/

//using std::vector;

 struct direction {
  float xhat;
  float yhat;
  float zhat;
  };

 struct skinny {
  std::vector<direction> vecDirection ;
  float origin[3];
  float distance;
  };

 struct fat {
  std::vector<direction> vecDirection ;
  float origin[3];
  float distance;
  float color[3];
  int pixel ;
  };



int main(){
#ifdef USE_TAU
  TAU_START("main");
  TAU_PROFILE_SET_NODE(0);
#endif
#ifdef SET_N
  int N=SET_N;
#else
  int N=1000000;
#endif

#ifdef VEC


  std::vector<float> v1;
  std::cout << "This is std::vector push_back time" << std::endl;

  {
    boost::timer::auto_cpu_timer t;
#ifdef USE_TAU
  TAU_START("std::vector.push_back loop");
#endif
  {
  for (int i = 0; i < N; ++i)
    {
        v1.push_back(float(i));
    }
  }
  }
#ifdef USE_TAU
  TAU_STOP("std::vector.push_back loop");
#endif
#ifdef USE_TAU
  TAU_START("std::vector.pop_back loop");
#endif
  std::cout << "This is std::vector pop_back time" << std::endl;
  {
  boost::timer::auto_cpu_timer t;
  {
  for (int i = 0; i < N; ++i)
    {
        v1.pop_back();
    }
  }
#ifdef USE_TAU
  TAU_STOP("std::vector.pop_back loop");
#endif

  }
#endif


#ifdef BOOST_VEC
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
#endif


#ifdef DEQ
  std::cout << "This is std::deque push_back time" << std::endl;
  {
    boost::timer::auto_cpu_timer t;
    std::deque <float> v4;
#ifdef USE_TAU
  TAU_START(" std::deque.push_back loop");
#endif
    for (int i = 0; i < N; ++i)
    {
        v4.push_back(float(i));
    }
#ifdef USE_TAU
  TAU_STOP(" std::deque.push_back loop");
#endif
  }
#endif

#ifdef USE_TAU
  TAU_STOP("main");
#endif

	return 0;
}


