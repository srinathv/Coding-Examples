/* matrix-tbb.cpp */
#include <tbb/parallel_for.h>
#include <tbb/blocked_range.h>
#include <iostream>
#include "tbb/tbb.h"


using namespace tbb;
using namespace std;

const int size = 1000;

float a[size][size];
float b[size][size];
float c[size][size];


class Multiply
{
public:
    void operator()(blocked_range<int> r) const {
      std::cout << "This threadID inside parallel_for is " << tbb::this_tbb_thread::get_id() << std::endl;
      std::cout << " " << std::endl;
        for (int i = r.begin(); i != r.end(); ++i) {
            for (int j = 0; j < size; ++j) {
                for (int k = 0; k < size; ++k) {
                    c[i][j] += a[i][k] * b[k][j];
                }
            }
        }
    }
};


int main()
{
  //Initialize buffers.
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      a[i][j] = (float)i + j;
      b[i][j] = (float)i - j;
      c[i][j] = 0.0f;
      }
  }

  // Compute matrix multiplication.
  // C <- C + A x B
  parallel_for(blocked_range<int>(0,size), Multiply());

  return 0;
}
