#include <iostream>
#include <string>

int main () {

  std::string username;
  std::string password;

  std::cout << " Enter your username: " << "\n" ;
  getline (std::cin, username, '\n');

  std::cout << " Enter your password : " <<  "\n";
  getline (std::cin, password, '\n');

  if (username == "root" && password == "xyzzy") {
    std::cout << "Access allowed " << "\n" ;
  }
  else {
    std::cout << "Bad username or password " <<  "\n" ;
    // returning is a convenient way to stop a program
    return 0;
  }
}
  
