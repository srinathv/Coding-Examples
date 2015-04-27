#include <vector>
#include <iostream>

struct test {
    std::vector<int> vec;
    std::vector<float> floatVec(3); //does not compile
};

//std::vector<float> floatVec(3);  //compiles

test mytest;

void foo() {
    mytest.vec.push_back(3);
}

int main(int argc, char** argv)
{
   foo();
   if (!mytest.vec.empty())  // it's always good to test container is empty or not
   {
     std::cout << "Vector Element" << mytest.vec[0] << std::endl;
   }
   return 0;
}
