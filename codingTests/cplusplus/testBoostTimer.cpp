#include <boost/timer/timer.hpp>
#include <cmath>

int main()
{
  boost::timer::auto_cpu_timer t;

  for (long i = 0; i < 100000000; ++i)
    std::sqrt(123.456L); // burn some time

  return 0;
}
