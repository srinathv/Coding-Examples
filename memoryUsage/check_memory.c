#include "memory.h"

int main()
{
    int n = 20;
    int entrySize = 10000000;
    int* buffer[n];
    long vmrss, vmsize;

    for (int i = 0; i < n; i++)
    {
        buffer[i] = malloc( entrySize * sizeof(int) );

        if (!buffer[i])
        {
            printf("Couldn't allocate memory!\n");
            exit(1);
        }

        for (int j = 0; j < entrySize; j++)
        {
            buffer[i][j] = 0;
        }

        get_memory_usage_kb(&vmrss, &vmsize);
        printf("%2d: Current memory usage: VmRSS = %6ld KB, VmSize = %6ld KB\n",
            i, vmrss, vmsize);
    }

    return 0;
}

