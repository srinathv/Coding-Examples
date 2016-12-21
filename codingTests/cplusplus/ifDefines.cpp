#include <iostream>
using namespace std;


#if defined(MACRO1) || defined(MACRO2)
#define TESTMACRO
#endif

int main() {
#if defined(TESTMACRO)
cout << "TESTMACRO on" << endl;
#else
cout << "TESTMACRO off" << endl;
#endif
}
