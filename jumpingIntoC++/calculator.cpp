#include<iostream>

int main() {

  int firstArgument;
  int secondArgument;

  std::cout << " Enter first argument: ";
  std::cin >> firstArgument;

  std::cout << " Enter second arugment: ";
  std::cin >> secondArgument;

  std::cout << firstArgument << " * " << secondArgument << " = " << firstArgument*secondArgument << std::endl;
  std::cout << firstArgument << " + " << secondArgument << " = " << firstArgument+secondArgument << std::endl;
  std::cout << firstArgument << " / " << secondArgument << " = " << firstArgument/secondArgument << std::endl;
  std::cout << firstArgument << " - " << secondArgument << " = " << firstArgument-secondArgument << std::endl; 
}
