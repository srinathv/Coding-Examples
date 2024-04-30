#include <limits>
#include <iostream>


int main()
{

int imin = std::numeric_limits<int>::min(); // minimum value
int imax = std::numeric_limits<int>::max();

std::cout<< "int min is "<< imin << "\n";
std::cout << "int max is "<< imax << "\n";


int64_t imin64 = std::numeric_limits<int64_t>::min(); // minimum value
int64_t imax64 = std::numeric_limits<int64_t>::max();
std::cout<< "int64_t min is "<< imin64 << "\n";
std::cout << "int64_t max is "<< imax64 << "\n";
}
