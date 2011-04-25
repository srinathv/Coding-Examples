#include <iostream>
#include <sstream>
#include <string>

int main(int argc, char** argv) {
  std::string type, name("what"), foo("");

  type = "your mom";
  foo = "why is this broken?";

  std::cout << type << " " << foo << std::endl;

  std::ostringstream lol;

  lol << type << " " << name;
  std::cout << lol.str();

  return 0;
}
