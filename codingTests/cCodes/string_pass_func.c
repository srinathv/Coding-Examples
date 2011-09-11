#include <stdio.h>
#include <string.h>


void printCatName(const char *partName){

  char fullname[20];
  char addname[4]=" bi";
  strcat(fullname, partName);
  strcat(fullname, addname );

  printf(" The full name is %s.\n", fullname);

  //fullname[0]='\0';
  memset(&fullname[0], 0, sizeof(fullname));
  memset(&addname[0], 0, sizeof(addname));

}


int main(){

  const char* name = "Dexter";
  const char* name2 = "Charles";

  printCatName(name);
  printCatName(name2);

  return 0;
}
