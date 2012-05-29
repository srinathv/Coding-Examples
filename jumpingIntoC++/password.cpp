
#include <iostream>
#include <string>

int main() {

  std::string password; // this will store the password

  std::cout << "Enter your password :" << "\n" ;
  getline (std::cin, password, '\n' );
  if (password == "xyzzy" ) {
    std::cout << "Access allowed " << "\n" ;
  }
  else {
    std::cout << "Bad passowrd.  Denied Access." << "\n" ;
    // returning is a convenient way to stop the program
    return 0;
  }
}
  
