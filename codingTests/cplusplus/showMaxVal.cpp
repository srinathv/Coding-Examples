#include <limits>
#include <iostream>


int main()
{

int imin = std::numeric_limits<int>::min(); // minimum value
int imax = std::numeric_limits<int>::max();

std::cout<< "int min is "<< imin << "/n";
std::cout << "int max is "<< imax << "/n";


}
