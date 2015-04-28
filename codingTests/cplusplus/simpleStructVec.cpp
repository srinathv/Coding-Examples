#include <vector>
#include <iostream>

struct direction {
  float xhat;
  float yhat;
  float zhat;
  };


struct test {
    std::vector<direction> vec;
    int length;
    };


void foo(test &directionVec, direction &whichway) {
  for (int i= 0; i< 201; i++)
  {
   directionVec.vec.push_back(whichway);
   directionVec.length =2 ;
  }
}






void vecFoo(std::vector<direction> &vecOfVecs, test &insideVec, direction &whichway) {
  for (int i = 0 ; i < 2; i++)
  {
      vecOfVecs.push_back(insideVec);
//        foo(&vecOfVecs[i]);
  }
}


int main(int argc, char** argv)
{

   test mytest;
   std::vector<test> myVecTest;

   direction thisway = {1,2,3};

   foo(mytest,thisway);
   if (!mytest.vec.empty())  // it's always good to test container is empty or not
   {
     std::cout << "Vector Element 200 xhat " << mytest.vec[200].xhat << std::endl;
     std::cout << "Vector Element 200 zhat " << mytest.vec[200].zhat << std::endl;
     std::cout << "Vector capacity " << mytest.vec.capacity() << std::endl;
     std::cout << "Vector Element 255 zhat " << mytest.vec[255].zhat << std::endl;
     std::cout << "Vector Element 256 zhat " << mytest.vec[256].zhat << std::endl;
   }

//   vecFoo(myVecTest,mytest, thisway);
//   if (!myVecTest.empty())
//   {
//    std::cout << "myVecTest " << myVecTest[1].vec[0].xhat << std::endl;
//   }
//   return 0;
}
