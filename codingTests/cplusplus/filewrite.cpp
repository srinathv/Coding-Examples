#include <iostream>
#include <string>
#include <fstream>
using namespace std;

int main()
{

ofstream File("File.txt");
string array[10];  


for (int x = 0; x <= 9; x++)
{
cout<< x <<" Enter a string:"<<endl;
cin>> array[x];
File<< array[x] << ", ";
cout<<endl;
}


return 0;
}

