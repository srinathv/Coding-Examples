#include <iostream>
#include <sstream>
#include <string>

int main() {
#if defined(__linux__)
  std::cout << "on linux \n";
#elif defined(__APPLE__)
  std::cout << "on APPLE \n";
#endif

#if defined (__x86_64__)
  std::cout << " on __x86_64__ \n";
#endif

#if defined (__i386__)
  std::cout << " on __i386__ \n";
#endif

#if defined (__GNUC__)
  std::cout << " using gnu c compiler \n";
#endif



  return 0;
}
