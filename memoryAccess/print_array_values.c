//from http://www.visitusers.org/index.php?title=C_vs_Fortran_memory_order
//
//

#include <stdio.h>

void
print_array_values(const int *values, const int *nvalues)
{
    int i, c = 0;
    for(i = 0; i < *nvalues ; i++)
    {
        printf("%d, ", values[i]);
        if(++c >= 10)
        {
            c = 0;
            printf("\n");
        }
    }
    printf("\n");
}

/* Called from Fortran (the trailing underscore makes it easier to link) */
void
print_array_values_(const int *values, const int *nvalues)
{
    print_array_values(values, nvalues);
}
