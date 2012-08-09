#include <pthread.h>
#include <stdio.h>
int main()
{
printf( "int: %d\n", sizeof( int));
printf( "pthread: %d\n", sizeof(pthread_t));
printf( "pthread key: %d\n", sizeof(pthread_key_t));
return 0;
}
