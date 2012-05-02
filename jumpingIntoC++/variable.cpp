#include <iostream>


int main() {

  int input;

  std::cout << "Enter in a number :";
  std::cin >> input;

  if (input < 10) {
    std::cout << "You entered a number less than 10 " << std::endl;
    }
    else{
    std::cout << "You entered a number greater than 10 " << std::endl;
  }
}
