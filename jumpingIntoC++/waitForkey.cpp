
/* this program will print something, wait for a keystroke, the print more */

#include <iostream>

  int main() {


    std::cout << " I want you to return to see my friends names \n";

    std::cin.get();

    std::cout << "Ryan and Ryan \n";
    std::cout << "Press return again to see another friend \n";
    
    std::cin.get(); // seems to expect a return

    std::cout << " Here is Jeremy and Brad.\n";



  }
