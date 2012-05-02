#include <iostream>
#include <string>


int main () {

  std::string phrase;
  std::string up2comma;


  std::cout << "Type in a phrase: ";
  std::getline(std::cin,phrase,"\n");
  std::cout << "Type in a phrase with a comma: ";
  std::getline(std::cin,up2comma,",");

  std::cout << " Your phrase : " << phrase << std::endl ;
  std::cout << " Your phrase up to comma : " << up2comma << std::endl ;

  
//not compiling .. where does getline live?

}


