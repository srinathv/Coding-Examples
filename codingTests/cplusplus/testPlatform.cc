#include <iostream>
#include <sstream>
#include <string>

int main() {
#if defined(__linux__)
  std::cout << "on linux \n";
#elif defined(__APPLE__)
  std::cout << "on APPLE \n";
#endif

  return 0;
}
