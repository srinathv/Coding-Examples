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

//std::vector<float> floatVec(3);  //compiles

test mytest;
std::vector<test> myVecTest;

direction thisway = {1,2,3};


void foo() {
  for (int i= 0; i< 201; i++)
  {
   mytest.vec.push_back(thisway);
  }
}


void vecFoo() {
  for (int i = 0 ; i < 2; i++)
  {
//    myVecTest.push_back(test()); //initialize first element which is a struct
//    myVecTest[0].length=2;
//    myVecTest[0].vec.push_back(thisway);
//    myVecTest.push_back(test()); // add second element
//    myVecTest[1].length=3;
//    myVecTest[1].vec.push_back(thisway);
    myVecTest.push_back(test()); //initialize first element which is a struct
    myVecTest[i].length=2;
    myVecTest[i].vec.push_back(thisway);
  }
}
int main(int argc, char** argv)
{
   foo();
   if (!mytest.vec.empty())  // it's always good to test container is empty or not
   {
     std::cout << "Vector Element 200 xhat " << mytest.vec[200].xhat << std::endl;
     std::cout << "Vector Element 200 zhat " << mytest.vec[200].zhat << std::endl;
     std::cout << "Vector capacity " << mytest.vec.capacity() << std::endl;
     std::cout << "Vector Element 255 zhat " << mytest.vec[255].zhat << std::endl;
     std::cout << "Vector Element 256 zhat " << mytest.vec[256].zhat << std::endl;
   }

   vecFoo();
   if (!myVecTest.empty())
   {
    std::cout << "myVecTest " << myVecTest[1].vec[0].xhat << std::endl;
   }
   return 0;
}
