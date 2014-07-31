#include <stdio.h>

 void * hello ( )
 {
 printf ( " Hello World from pthread ! \ n" ) ;
 return NULL ;
 }

 void main ( )
 {
 pthread_t thread ;

 pthread_create(&thread , NULL, &hello , NULL ) ;

 printf ( " Hello World from main ! \ n" ) ;

 pthread_join ( thread , NULL ) ;
 }
