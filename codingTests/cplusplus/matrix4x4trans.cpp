#include <iostream>
using namespace std;

int main()
{
    double a[4][4], trans[4][4];
    int   r = 4, c = 4;
    int  i, j;


    // Storing element of matrix entered by user in array a[][].
    for(i = 0; i < r; ++i)
    for(j = 0; j < c; ++j)
    {
        a[i][j] = rand() % 10 + 1.1;
    }

    // Displaying the matrix a[][]
    cout << endl << "Entered Matrix: " << endl;
    for(i = 0; i < r; ++i)
        for(j = 0; j < c; ++j)
        {
            cout << " " << a[i][j];
            if(j == c - 1)
                cout << endl << endl;
        }

 #ifdef NEON

    // Want to do VTRN,64 d0,d1, then VTRN,64 d2,d3, then VTRN,64 q0,q1

    // Finding transpose of matrix a[][] and storing it in array trans[][].
    for(i = 0; i < r; ++i)
        for(j = 0; j < c; ++j)
        {
            trans[j][i]=a[i][j];
        }

    // Displaying the transpose,i.e, Displaying array trans[][].
    cout << endl << "Transpose of Matrix: " << endl;
    for(i = 0; i < c; ++i)
        for(j = 0; j < r; ++j)
        {
            cout << " " << trans[i][j];
            if(j == r - 1)
                cout << endl << endl;
        }

    return 0;
}
