
#include <iostream>
#include <string>

int main () {
  
  int age1, age2;

  std::cout << "Please enter your age: " << "\n";
  std::cin >> age1;

   
  std::cout << "Please enter someone else's age: " << "\n";
  std::cin  >> age2;


  if (age1 > 100 || age2 > 100) {
    std::cout << "One of you are old! " << "\n";
  }
  else if (age1 < age2 ) {

    std::cout << "You are younger than that other person." << "\n";
  }
  else if (age2 < age1) { 
    
    std::cout << "You are older than that other person." << "\n" ;
  }


}
