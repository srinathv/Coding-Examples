
#include <iostream>
#include <vector>

#ifdef SET_N
  int N=SET_N;
#else
  int N=1000000;
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

//could have a function that grows the direction vector but no need for now.


  void makeVecOfSkinny(std::vector<skinny> &vecOfSkinny, skinny &singleRay, direction &whichway,
                float origin[3], float &distance ) {
    for (int i = 0 ; i < N+1; i++)
    {
    vecOfSkinny.push_back(singleRay);
    vecOfSkinny[i].vecDirection.push_back(whichway);
    vecOfSkinny[i].origin[0] = origin[0];
    vecOfSkinny[i].origin[1] = origin[1];
    vecOfSkinny[i].origin[2] = origin[2];
    vecOfSkinny[i].distance = distance;
    }
}

int main(){


  direction normal = {1.1,3.45,2.2};
  float myOrigin[3] = { 20.1, 22.2, 33.3};
  float distance = 4533.23422;

  std::vector<skinny> myVec;
  skinny singleRay;

  makeVecOfSkinny(myVec, singleRay, normal, myOrigin, distance);

  if (!myVec.empty()) {
    std::cout << "what is inside " << myVec[2].origin[2] << std::endl;
  }

	return 0;
}


